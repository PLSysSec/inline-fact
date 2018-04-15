{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.FaCT.Inline {-(
    fact
  , link
  )-} where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Word
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.Directory (removeFile)
import System.FilePath (replaceExtension)
import System.IO
import System.Posix.Temp
import System.Process
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Number (nat)
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Posix.DynamicLinker as L

-- | Declare a top-level fact function that can be called from
-- Haskell.
fact :: QuasiQuoter
fact = QuasiQuoter { quoteDec  = qDec
                   , quoteExp  = error "undefined"
                   , quotePat  = error "undefined"
                   , quoteType = error "undefined"
                   }

qDec :: String -> Q [Dec]
qDec source = do
  runIO $ putStrLn source
  fun@(Fun name _ _ isExternal) <- runIO $ parseFaCTFun source
  let source' = if isExternal then source' else "export " ++ source
  ofile <- runIO $ compile name source'
  return $ funToDecs ofile fun

compile :: String -> String -> IO FilePath
compile name source = do
  bracket (mkstemps ("/tmp/" ++ name) ".fact")
          (\(file, h) -> do hClose h
                            callProcess factCC [file] 
                            -- REMOVE all but the .so file
                            removeFile file)
          $ \(file, h) -> do
           putStrLn $ "Writing to..." ++ file
           hPutStrLn h source
           return $ replaceExtension file "fpic.so"

factCC :: FilePath
factCC = "fact"

dlopen :: FilePath -> IO L.DL
dlopen ofile = do
  res <- L.dlopen ofile [L.RTLD_LAZY, L.RTLD_GLOBAL]
  case res of
    L.DLHandle _ -> return res
    _ -> L.dlerror >>= fail

getDynFunction :: FilePath -> String -> IO (FunPtr a)
getDynFunction ofile name = do
   dl <- dlopen ofile
   L.dlsym dl name

--
-- Function declarations
--

type Iden = String

data Fun = Fun { funName    :: Iden,
                 funRetTy   :: RetTy,
                 funArgs    :: [Arg],
                 isExternal :: Bool
               } deriving (Eq, Show, Read)

data Arg = Arg { argName  :: Iden,
                 argMut   :: Mut,
                 argLabel :: Label,
                 argTy    :: Ty
               } deriving (Eq, Show, Read)

data Mut = Mut | Const
         deriving (Eq, Show, Read)

data RetTy = RetVoid | RetTy Label BaseTy
           deriving (Eq, Show, Read)

data Ty = TBaseTy BaseTy
        | TArrayTy BaseTy (Maybe Int)
        deriving (Eq, Show, Read)

data BaseTy = TUInt Int
            | TBool
            deriving (Eq, Show, Read)

data Label = Secret | Public
           deriving (Eq, Show, Read)

--
-- Parser code
--


ident :: Parser Iden
ident = do { c <- id0; cs <- many idchar; return (c:cs) }
 where id0    = letter <|> char '_'
       idchar = id0 <|> digit

mut :: Parser Mut
mut = do
  str <- string "mut" <|> string "const"
  return $ case str of
    "mut" -> Mut
    _ -> Const

label :: Parser Label
label = do
  str <- string "secret" <|> string "public"
  return $ case str of
    "secret" -> Secret
    _ -> Public

baseTy :: Parser BaseTy
baseTy = do
  boolTy <|> uintTy
    where boolTy = string "bool" >> return TBool
          uintTy = do void $ string "uint"
                      n <- nat
                      return $ TUInt n

ty :: Parser Ty
ty = try arrayTy <|> baseTy'
  where baseTy' = baseTy >>= return . TBaseTy
        arrayTy = do bTy <- baseTy
                     spaces
                     void $ char '['
                     spaces
                     len <- optionMaybe nat
                     spaces
                     void $ char ']'
                     return $ TArrayTy bTy len

retTy :: Parser RetTy
retTy = void' <|> retTy'
  where void' = string "void" >> return RetVoid
        retTy' = do l <- label
                    spaces
                    t <- baseTy
                    return $ RetTy l t

arg :: Parser Arg
arg = do
  l <- label
  spaces
  m <- do mm <- optionMaybe mut
          spaces
          return mm
  t <- ty
  spaces
  i <- ident
  return $ Arg { argName  = i
               , argMut   = maybe Const id m
               , argLabel = l
               , argTy    = t }

fun :: Parser Fun
fun = do
  mextrn <- optionMaybe $ spaces >> string "export"
  spaces
  t <- retTy
  spaces
  i <- ident
  spaces
  void $ char '('
  spaces
  as <- arg `sepBy` (spaces >> (void $ char ',') >> spaces)
  spaces
  void $ char ')'
  return $ Fun { funName  = i
               , funRetTy = t
               , funArgs  = as
               , isExternal = isJust mextrn }


parseFaCTFun :: String -> IO Fun
parseFaCTFun str = do
  case runParser fun () "" str of
    Left pe -> fail $ show pe
    Right f -> return f


class ToTHTy ty where
  toTy :: ty -> [TH.Type]
  toTy ty = [toTy0 ty]
  toTy0 :: ty -> TH.Type
  toTy0 = head . toTy

instance ToTHTy BaseTy where
  toTy0 ty = TH.ConT $ case ty of
                TBool    -> ''Word8
                TUInt 8  -> ''Word8
                TUInt 16 -> ''Word16
                TUInt 32 -> ''Word32
                TUInt 64 -> ''Word64
                _ -> error $ "unsupported type " ++ show ty


instance ToTHTy Arg where
  toTy (Arg _ Const _ (TBaseTy ty))       = toTy ty
  toTy (Arg _ Mut   _ (TBaseTy ty))       = [TH.AppT (TH.ConT ''Ptr) (toTy0 ty)]
  toTy (Arg _ _ _ (TArrayTy ty (Just _))) = [TH.AppT (TH.ConT ''Ptr) (toTy0 ty)]
  toTy (Arg _ _ _ (TArrayTy ty Nothing))  = [TH.AppT (TH.ConT ''Ptr) (toTy0 ty), toTy0 (TUInt 32)]


instance ToTHTy RetTy where
  toTy0 RetVoid      = TH.AppT (TH.ConT ''IO) (TH.ConT ''())
  toTy0 (RetTy _ ty) = TH.AppT (TH.ConT ''IO) (toTy0 ty)

funTy :: Fun -> TH.Type
funTy (Fun _ rTy args _) = 
  let argTys = concatMap toTy args 
      retTy  = toTy0 rTy
  in foldr (\a b -> (TH.AppT (TH.AppT TH.ArrowT a) b)) retTy argTys

funToDecs :: FilePath -> Fun -> [TH.Dec]
funToDecs ofile fun@(Fun name _ _ _) =
  let ty = funTy fun
      ffiImportD = ffiImport fun ty
  in [ 
    -- foreign import ccall "dynamic" mkFun_add :: FunPtr (Word32 -> Word32 -> IO Word32) -> Word32 -> Word32 -> IO Word32
      ffiImportD
    -- add :: Word32 -> Word32 -> IO Word32
    , TH.SigD (TH.mkName name) ty
    -- add x y = do p_add <- getDynFunction "/home/d/hack/inline-fact/cswap.so" "add" 
    --                       mkFun_add p_add x y
    , funDec ofile fun
    ]


ffiImport :: Fun -> Type -> TH.Dec
ffiImport (Fun name _ _ _) ty = 
  TH.ForeignD $ TH.ImportF TH.CCall TH.Unsafe "dynamic" 
                      (TH.mkName $ "mkFun_" ++ name) $
                      (TH.AppT (TH.AppT TH.ArrowT (TH.AppT (TH.ConT ''FunPtr) ty)) ty)

funDec :: FilePath -> Fun -> TH.Dec
funDec ofile (Fun name _ args0 _) = 
  let nrArgs     = length $ concatMap toTy args0
      args       = map (\nr -> TH.mkName $ "arg" ++ show nr) [1..nrArgs]
      argPats    = map TH.VarP args
      --
      getFuncAct = TH.AppE (TH.AppE (TH.VarE 'getDynFunction) (TH.LitE $ TH.StringL ofile))
                           (TH.LitE $ TH.StringL name)
      --
      mkFun      = TH.mkName $ "mkFun_" ++ name
      p_fun      = mkName $ "p_" ++ name
      mkFunAct   = foldl TH.AppE (TH.AppE (TH.VarE mkFun) (TH.VarE p_fun)) $ map TH.VarE args
      --
      body       = TH.NormalB $ TH.DoE [ TH.BindS (TH.VarP p_fun) getFuncAct
                                         , TH.NoBindS mkFunAct]
  in TH.FunD (TH.mkName name) [ TH.Clause argPats body [] ]
