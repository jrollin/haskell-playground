-- program need main func to compile with GHC

main :: IO()
main = do
   print "Who is the email for?"
   recipient <- getLine
   print "What is the Title?"
   title <- getLine
   print "Who is the Author?"
   author <- getLine
   print (createEmail recipient title author)

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart author = "Thanks,\n"++author
toPart recipient = "Dear " ++ recipient ++ ",\n"

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
           GHCi—Haskell’s Interactive Read-Eval-Print Loop (REPL)                              fromPart author

