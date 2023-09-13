module Lcc.Codegen where

import Data.List (intercalate)
import Numeric (showHex)

import System.Process (readProcess)

import Lcc.IR
  (
    Dat (Arg, Bin, Ref, Ret, Unb)
  , IR (Proc, Start)
  , Ins (Cal, End, Sav)
  , Spool (Spool)
  )

data X86
  = Global String
  | Section Name [Block]

data Block
  = Label Name [Line]
  | Text [Line]

type Name = String
type Line = String

instance Show X86 where
  show (Global n) = "global " <> n
  show (Section n ls) = "section " <> n <> "\n" <> intercalate "\n\n" (fmap show ls)

instance Show Block where
  show (Label n is) = n <> ":" <> "\n" <> intercalate "\n" (fmap offseti is)
  show (Text is) = intercalate "\n" (fmap offseti is)

x86 :: Spool IR -> Spool X86
x86 (Spool ls) = Spool
  [
    Global "_start"
  , Section ".bss" bss
  , Section ".text" $ fmap label ls
  ]
 where
  bss = pure . Text $ "R:      resb 2" : (filter (not . null) $ fmap unbinded ls)
  label l = case l of
             Proc s is -> Label ("_" <> show s) $ procedure is
             Start is -> Label "_start" $ start is
  procedure = (<> pure "ret") . concatMap instr
  start is = intercalate (pure mempty) $ concatMap instr is : [result, write, exit]
  result =
    [
      "mov     [R], rax"
    , "mov     byte [R+1], 0x0A"
    ]
  write =
    [
      "mov     rax, 1"
    , "mov     rdi, 1"
    , "mov     rsi, R"
    , "mov     rdx, 2"
    , "syscall"
    ]
  exit = 
    [
      "mov     rax, 60"
    , "xor     rdi, rdi"
    , "syscall"
    ]
  unbinded (Proc n _) = "U" <> show n <> ":     resq 1"
  unbinded _ = mempty

instr :: Ins -> [Line]
instr i = case i of
            Cal f x -> [
                         "mov     rsi, " <> loa x
                       , "mov     rdi, " <> loa f
                       , "call    rdi"
                       ]
            End d -> if ret d then mempty else pure $ "mov     rax, " <> loa d
            Sav u -> pure $ "mov     " <> loa u <> ", rsi"
 where
  loa d = case d of
            Arg -> "rsi"
            Bin l -> "[U" <> show l <> "]"
            Ref l -> "_" <> show l
            Ret -> "rax"
            Unb c -> "0x" <> showHex (fromEnum c) mempty
  ret Ret = True
  ret _ = False

offseti :: String -> String
offseti s = if null s || last s == ':' then s else replicate 8 ' ' <> s

run' :: Spool IR -> IO ()
run' s = do
  let a = x86 s
  print a
  writeFile "/tmp/lcc.s" $ show a
  readProcess "nasm" ["-g", "-f", "elf64", "/tmp/lcc.s", "-o", "/tmp/lcc.o"] mempty >>= putStrLn
  readProcess "ld" ["/tmp/lcc.o", "-o", "/tmp/a.out"] mempty >>= putStrLn
  readProcess "/tmp/a.out" mempty mempty >>= putStrLn
