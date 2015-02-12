-- file: ch01/WC.hs

main = interact lineCount
    
lineCount input = show (length (lines input)) ++ "\n"

wordCount input = show (length (words input)) ++ "\n"

charCount input = show (length input) ++ "\n"