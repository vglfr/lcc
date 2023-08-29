module Lcc.Codegen where

import Data.List (intercalate)
import Numeric (showHex)

import System.Process (readProcess)

import Lcc.IR
  (
    Dat (Arg, Ref, Ret, Val)
  , IR (Proc, Start)
  , Ins (Cal, Loa)
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
x86 (Spool n ls) = Spool n
  [
    Global "_start"
  , Section ".data" data'
  , Section ".text" $ fmap label ls
  ]
 where
  data' = pure . Text . pure $ "_R:     db 0x0, 0x0"
  label l = case l of
             Proc k is -> Label ('_' : show k) $ procedure is
             Start is -> Label "_start" $ start is
  procedure is = intercalate (pure mempty) $ enter : concatMap instr is : pure leave
  start is = intercalate (pure mempty) $ concatMap instr is : [result, write, exit]
  enter =
    [
      "push    rbp"
    , "mov     rbp, rsp"
    ]
  leave =
    [
      "pop     rax"
    , "pop     rbp"
    , "ret"
    ]
  result =
    [
      "pop     word [_R]"
    , "mov     byte [_R+1], 0x0A"
    ]
  write =
    [
      "mov     rax, 1"
    , "mov     rdi, 1"
    , "mov     rsi, _R"
    , "mov     rdx, 2"
    , "syscall"
    ]
  exit = 
    [
      "mov     rax, 60"
    , "xor     rdi, rdi"
    , "syscall"
    ]

instr :: Ins -> [Line]
instr i = case i of
            Loa d -> pure $ "push    " <> loa d
            Cal n -> [
                       "call    [rsp]"
                     , "add     rsp, " <> show (n * 8)
                     ]
 where
  loa d = case d of
            Val c -> "0x" <> showHex (fromEnum c) mempty
            Ref r -> "_" <> show r
            Arg a -> "qword [rbp+" <> show (24 + a*8) <> "]"
            Ret -> "rax"

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
