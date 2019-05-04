{-----------------------------------
|           Text Editor            |
|          Version: 1.30           |
|      Sam Conran (15590816)       |
-----------------------------------}

--Import some libraries. Please note: these are only used for the
--interface, NOT in the defining the TextEditor's data/operations
import System.Process
import System.Exit (exitSuccess)
import System.IO.Unsafe

--Text editor constructor. Takes in...
--The string of text, a cursor position integer, an tuple of two ints representing start/end
--of selection, two string lists representing undo/redo buffers, and a string for any
--copied text (the clipboard). For consistency, they will be generally represented as:
--                           str    cur sel         ubf      rbf     cbd
data TextEditor = TextEditor String Int (Int, Int) [String] [String] String deriving (Show)

--Function to create a text editor (filled with default values)
createTextEditor :: TextEditor
createTextEditor = (TextEditor "" 0 (0,0) [] [] "")

--Function to load a text file
loadFile :: TextEditor -> String -> IO(TextEditor)
--Takes in a filename. Loads text from that file.
loadFile (TextEditor str cur sel ubf rbf cbd) filename = do
    lText <- readFile filename
    {-returns the text editor with that string, adds the original str to the undo buffer,
    keeps the input clipboard, and sets default values for everything else-}
    return (TextEditor lText 0 (0,0) (ubf ++ [str]) [] cbd)

--Function to save the text from an editor to a text file
saveFile :: TextEditor -> String -> IO(TextEditor)
--Saves using the the input string argument as a filename
saveFile (TextEditor str cur sel ubf rbf cbd) filename = do
    writeFile filename str
    return (TextEditor str cur sel ubf rbf cbd)

--Function to add text at the cursor position
addText :: TextEditor -> String -> TextEditor
{-Takes in text editor and input string. Splits the string at the cursor position into two strings (h and t). 
New editor string is h + input string + t. Adds old str to the undo buffer.-}
addText (TextEditor str cur sel ubf rbf cbd) val = (TextEditor (let (h,t) = splitAt cur str in h ++ (val ++ t)) cur sel (ubf ++ [str]) rbf cbd)

--Function to delete text either at cursor position or in selection
deleteText :: TextEditor -> TextEditor
deleteText (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection, simply split at cursor (h,t), then remove the first element from t and concatenate the two
    | (fst sel == 0 && snd sel == 0) = (TextEditor (let (h,t) = splitAt cur str in h ++ tail t) cur sel (ubf ++ [str]) rbf cbd)
    --If there is a selection, split (h,t) at the selection start then drop the (selection start - end) number of elements from t before concatenating the two.
    | otherwise = (TextEditor (let (h,t) = splitAt (fst sel) str in h ++ (drop (snd sel - fst sel + 1) t)) cur (0,0) (ubf ++ [str]) rbf cbd)

--Function to clear the selection completely.
clearSelection :: TextEditor -> TextEditor
--Simply sets value to (0,0)
clearSelection (TextEditor str cur sel ubf rbf cbd) = (TextEditor str cur (0,0) ubf rbf cbd)

--Function to move cursor 1 position to the left
moveCursorLeft :: TextEditor -> TextEditor
moveCursorLeft (TextEditor str cur sel ubf rbf cbd)
    --If the cursor is above 0, simply take 1 away
    | cur > 0   = (TextEditor str (cur - 1) sel ubf rbf cbd)
    --If not, simply return the original input
    | otherwise = (TextEditor str cur sel ubf rbf cbd)

--Function to move the cursor 1 position to the right
moveCursorRight :: TextEditor -> TextEditor
moveCursorRight (TextEditor str cur sel ubf rbf cbd)
    --If the cursor is less to the length of the string (thus not at the end), add 1
    | cur < (length str) = (TextEditor str (cur + 1) sel ubf rbf cbd)
    --Otherwise, it is at the end so just return the input
    | otherwise          = (TextEditor str cur sel ubf rbf cbd)

--Function to select one character to the left
selectLeft :: TextEditor -> TextEditor
selectLeft (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection  and the cursor is already at 0, return input
    | cur == 0 && (fst sel == 0 && snd sel > 0) = (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection, set the selection to (cursor position - 1, cursor position)
    | fst sel == 0 && snd sel == 0 = (TextEditor str cur (cur - 1, cur) ubf rbf cbd)
    --If there is already a selection, simply take 1 away from the selection start
    | otherwise = (TextEditor str cur (fst sel - 1, snd sel) ubf rbf cbd)

--Function to select one character to the right
selectRight :: TextEditor -> TextEditor
selectRight (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection  and the cursor is already at the end (length of str), return input
    | cur == length str && (fst sel == 0 && snd sel > 0) = (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection, set the selection to (cursor position, cursor position + 1)
    | fst sel == 0 && snd sel == 0 = (TextEditor str cur (cur, cur + 1) ubf rbf cbd)
    --If there is already a selection, simply add 1 to the selection end
    | otherwise = (TextEditor str cur (fst sel, snd sel + 1) ubf rbf cbd)

--Function to set the cursor to the start of the string
goToStartOfString :: TextEditor -> TextEditor
--Sets cursor position to 0
goToStartOfString (TextEditor str cur sel ubf rbf cbd) = (TextEditor str 0 sel ubf rbf cbd)

--Function to set the cursor to the end of string
goToEndOfString :: TextEditor -> TextEditor
--Sets cursor position to length of string
goToEndOfString (TextEditor str cur sel ubf rbf cbd) = (TextEditor str (length str) sel ubf rbf cbd)

--Recursive Function to jump the cursor to the start of the last ord
--(Evaluates (cur - 2) instead of cur to help avoid cases where it starts on a ' ')
jumpToStartPrevious :: TextEditor -> TextEditor
jumpToStartPrevious (TextEditor str cur sel ubf rbf cbd)
    --If the cur - 2 is less than 0 we're at the end of string so just return the input
    | cur - 2 < 0 = (TextEditor str 0 sel ubf rbf cbd)
    --If string at index (cur - 2) is equal to ' ', return with cur - 1 (start of next word)
    | str!!(cur - 2) == ' ' = (TextEditor str (cur - 1) sel ubf rbf cbd)
    --Otherwise, recursive call to the function with (cur - 1)
    | otherwise = jumpToStartPrevious (TextEditor str (cur - 1) sel ubf rbf cbd)

--Recursive Function to jump the cursor to the start of the next word
jumpToStartNext :: TextEditor -> TextEditor
jumpToStartNext (TextEditor str cur sel ubf rbf cbd)
    --If the cur >= str length we're at the end of string so just return the input
    | cur >= length str = (TextEditor str (length str) sel ubf rbf cbd)
    --If string at index (cur) is equal to ' ', return with cur + 1 (start of next word)
    | str!!(cur) == ' ' = (TextEditor str (cur + 1) sel ubf rbf cbd)
    --Otherwise, recursive call to the function with (cur + 1)
    | otherwise = jumpToStartNext (TextEditor str (cur + 1) sel ubf rbf cbd)

--Recursive function to select all the way to the start of the previous word
--(Evaluates (fst sel - 2) instead of (fst sel) to help avoid cases where it starts on a ' ')
selectPrevious :: TextEditor -> TextEditor
selectPrevious (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection and the cusrsor is currently at the end, just return input
    | (fst sel == 0 && snd sel == 0) && cur == 0 = (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection, recursively call self with a text editor with a selection either side of the cursor
    | fst sel == 0 && snd sel == 0 = selectPrevious (TextEditor str cur (cur - 1, cur) ubf rbf cbd)
    --If the selection start - 2 is at the start of the string, simply return a text editor with fst sel of 0
    | (fst sel - 2) < 0 = (TextEditor str cur (0, snd sel) ubf rbf cbd)
    --If the string at index (selection start - 2) is a space, return a text editor with fst sel - 1
    | str!!(fst sel - 2) == ' ' = (TextEditor str cur ((fst sel - 1), snd sel) ubf rbf cbd)
    --Otherwise recursively call with a text editor with 1 taken away from fst sel
    | otherwise = selectPrevious (TextEditor str cur ((fst sel - 1), snd sel) ubf rbf cbd) 

--Recursive Function to select all the way to the start of the next word
selectNext :: TextEditor -> TextEditor
selectNext (TextEditor str cur sel ubf rbf cbd)
    --If there's no selection and the cursor is at the end of the string, just return input
    | (fst sel == 0 && snd sel == 0) && cur == length str = (TextEditor str cur sel ubf rbf cbd)
    --If no selection then recurseively call self with a text editor with a selection either side of the cursor
    | fst sel == 0 && snd sel == 0 = selectNext (TextEditor str cur (cur, cur + 1) ubf rbf cbd)
    --If the selection end is >= the str length, just return text editor with selection ending with str length
    | snd sel >= length str = (TextEditor str cur (fst sel, length str) ubf rbf cbd)
    --If string at index of selection end is equal to ' ', return with the selection end + 1
    | str!!(snd sel) == ' ' = (TextEditor str cur (fst sel, (snd sel + 1)) ubf rbf cbd)
    --Otherwise, recursive call to the function with (snd sel + 1)
    | otherwise = selectNext (TextEditor str cur (fst sel, (snd sel + 1)) ubf rbf cbd)

--Function to select all characters to the left
--Either way, set selection start to 0
selectAllLeft :: TextEditor -> TextEditor
selectAllLeft (TextEditor str cur sel ubf rbf cbd)
    --If no selection, set selection end to the cursor position
    | (fst sel == 0 && snd sel == 0) = (TextEditor str cur (0, cur) ubf rbf cbd)
    --Otherwise, keep the selection end the same as input
    | otherwise = (TextEditor str cur (0, snd sel) ubf rbf cbd)

--Function to select all characters to the right
--Either way, set selection start to str length
selectAllRight :: TextEditor -> TextEditor
selectAllRight (TextEditor str cur sel ubf rbf cbd)
    --If no selection, set selection start to the cursor position
    | (fst sel == 0 && snd sel == 0) = (TextEditor str cur (cur, (length str)) ubf rbf cbd)
    --Otherwise, keep the selection start the same as input
    | otherwise = (TextEditor str cur (fst sel, (length str)) ubf rbf cbd)

--Function to select all text
selectAll :: TextEditor -> TextEditor
--Sets selection start to 0 and selection end to the length of str
selectAll (TextEditor str cur sel ubf rbf cbd) = (TextEditor str cur (0,(length str)) ubf rbf cbd)

--Function to copy the value of str between selection into the clipboard
copySelection :: TextEditor -> TextEditor
--Simply sets cbd to the text between (fst sel) and (snd sel)
copySelection (TextEditor str cur sel ubf rbf cbd) = (TextEditor str cur sel ubf rbf (take (snd sel - fst sel + 1) (drop (fst sel) str)))

--Function to cut the value of str between selection into the clipboard
cutSelection :: TextEditor -> TextEditor
--Sets cbd to the text between (fst sel) and (snd sel) and makes the output's str equal everything before selection + everything after selection
cutSelection (TextEditor str cur sel ubf rbf cbd) = (TextEditor (let (h,t) = splitAt (fst sel) str in h ++ (drop (snd sel - fst sel + 1) t)) cur (0,0) (ubf ++ [str]) rbf (take (snd sel - fst sel + 1) (drop (fst sel) str)))

--Function to paste the value of clipboard into the str at cursor position
paste :: TextEditor -> TextEditor
--Splits str at cursor position and then concatenates first half + paste value + second half, also adds input str to the undo buffer
paste (TextEditor str cur sel ubf rbf cbd) = (TextEditor (let (h, t) = splitAt cur str in h ++ (cbd ++ t)) cur sel (ubf ++ [str]) rbf cbd)

--Function to undo an action by replacing str with the previous value
undo :: TextEditor -> TextEditor
undo (TextEditor str cur sel ubf rbf cbd)
    --If nothing in undo buffer, just return input
    | ubf == [] = (TextEditor str cur sel ubf rbf cbd)
    {-Otherwise, output a text editor with str as the last element of the buffer,
    ubf as everything but the last element, and add input str to the redo buffer-}
    | otherwise = (TextEditor (last ubf) cur sel (init ubf) (rbf ++ [str]) cbd)

--Function to redo undone actions
redo :: TextEditor -> TextEditor
redo (TextEditor str cur sel ubf rbf cbd)
    --If nothing in buffer, just return input
    | rbf == [] = (TextEditor str cur sel ubf rbf cbd)
    {-Otherwise, set ouput str to be the last buffer element, add input str to the
    undo buffer, take the last string off the new redo buffer-}
    | otherwise = (TextEditor (last rbf) cur sel (ubf ++ [str]) (init rbf) cbd)


    {- Everything below this is interface design rather than operation definitions -}

--Function to display a prettified version of the text editor to the user
displayString :: TextEditor -> String
displayString (TextEditor str cur sel ubf rbf cbd)
    --If there is no selection, show the cursor as a '|'
    | (fst sel == 0 && snd sel == 0) = (let (h, t) = splitAt cur str in h ++ "|" ++ t) ++ "\n"
    --Otherwise, show the selection between '[' and ']'
    | otherwise = (let (h, t) = splitAt (fst sel) str in h ++ "[" ++ (let (h2, t2) = splitAt (snd sel - fst sel) t in h2 ++ "]" ++ t2)) ++ "\n"

--Function to accept user input and call above actions accordingly
opMenu :: TextEditor -> IO(TextEditor)
opMenu (TextEditor str cur sel ubf rbf cbd) = do
    c <- getLine
    --result r depends on user input, matches up with functions
    let r = case c of
            "-d" -> deleteText (TextEditor str cur sel ubf rbf cbd)
            "-cl" -> clearSelection (TextEditor str cur sel ubf rbf cbd)
            "-c" -> copySelection (TextEditor str cur sel ubf rbf cbd)
            "-x" -> cutSelection (TextEditor str cur sel ubf rbf cbd)
            "-v" -> paste (TextEditor str cur sel ubf rbf cbd)
            "-u" -> undo (TextEditor str cur sel ubf rbf cbd)
            "-r" -> redo (TextEditor str cur sel ubf rbf cbd)
            "->" -> moveCursorRight (TextEditor str cur sel ubf rbf cbd)
            "->>" -> jumpToStartNext (TextEditor str cur sel ubf rbf cbd)
            "->>>" -> goToEndOfString (TextEditor str cur sel ubf rbf cbd)
            "<-" -> moveCursorLeft (TextEditor str cur sel ubf rbf cbd)
            "<<-" -> jumpToStartPrevious (TextEditor str cur sel ubf rbf cbd)
            "<<<-" -> goToStartOfString (TextEditor str cur sel ubf rbf cbd)
            "-a" -> selectAll (TextEditor str cur sel ubf rbf cbd)
            "-]" -> selectRight (TextEditor str cur sel ubf rbf cbd)
            "-]]" -> selectNext (TextEditor str cur sel ubf rbf cbd)
            "-]]]" -> selectAllRight (TextEditor str cur sel ubf rbf cbd)
            "[-" -> selectLeft (TextEditor str cur sel ubf rbf cbd)
            "[[-" -> selectPrevious (TextEditor str cur sel ubf rbf cbd)
            "[[[-" -> selectAllLeft (TextEditor str cur sel ubf rbf cbd)
            ('-':'s':' ':s) -> unsafePerformIO (saveFile (TextEditor str cur sel ubf rbf cbd) s)
            ('-':'l':' ':s) -> unsafePerformIO (loadFile (TextEditor str cur sel ubf rbf cbd) s)
            input -> addText (TextEditor str cur sel ubf rbf cbd) input
    return r

--Recursive Function to define the main menu
mainMenu :: TextEditor -> IO()
mainMenu (TextEditor str cur sel ubf rbf cbd) = do
    --Clear screen
    system "cls"
    --Define and display prettified text editor
    let displayStr = displayString (TextEditor str cur sel ubf rbf cbd)
    putStrLn displayStr
    --Then call the option menu
    r <- opMenu (TextEditor str cur sel ubf rbf cbd)
    --Call self with result
    mainMenu r

--Main simply creates/initialises a text editor and passes it to the menu
main = do
    let init = createTextEditor
    mainMenu init