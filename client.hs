module Main where

import Qt -- By including all of Qt, my executable will be 
          -- fairly large

{--
import Qtc.Classes.Qccs
import Qtc.Classes.Qccs_h
import Qtc.Classes.Gui
import Qtc.ClassTypes.Gui
import Qtc.Gui.Base
import Qtc.Enums.Base
import Qtc.Enums.Classes.Core
import Qtc.Enums.Core.Qt
import Qtc.Gui.QApplication
import Qtc.Gui.QMessageBox
import Qtc.Gui.QLabel
import Qtc.Gui.QLabel_h
--}

type MyQPushButton = QPushButtonSc (CMyQPushButton)
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton b = qSubClass $ qPushButton b

updateName :: QLineEdit () -> QLabel () -> QDialog () -> MyQPushButton -> IO ()
updateName le la dia this = do
    theName <- text le ()
    setText la theName
    done dia (1::Int)
    return ()    

showUserName namelabel = do
    dlayout <- qVBoxLayout ()
    dialog <- qDialog ()

    la <- qLabel "Enter name"
    le <- qLineEdit ()
    pb <- myQPushButton "Done"

    addWidget dlayout la
    addWidget dlayout le
    addWidget dlayout pb

    setLayout dialog dlayout

    connectSlot pb "clicked()" pb "click()" $ updateName le namelabel dialog

    exec dialog ()

toggleButton :: MyQPushButton -> MyQPushButton -> IO ()
toggleButton aButton this = do
    status <- isEnabled aButton ()
    if status == True
        then do
            -- This should never get executed because the only way
            -- To push a button is if the other one is disabled
            -- Which implies the button pressed is already enabled
            -- Otherwise, it could not have been pressed :)
            putStrLn "aButton is enabled"
            setEnabled aButton False
            setEnabled this True
        else do
            buttontext <- text aButton ()
            putStr buttontext 
            putStr " is disabled\n"
            setEnabled aButton True
            setEnabled this False

initServerGui connectB disconnectB username chatDisplay mainWindow = do
    -- Main vertical box layout
    vlayout <- qVBoxLayout ()

    -- First row holds username information
    namelabel <- qLabel "Username:"
    setFixedWidth namelabel (80::Int)

    userlayout <- qHBoxLayout ()

    addWidget userlayout namelabel
    addWidget userlayout username
    setAlignment userlayout (fAlignLeft::Alignment)

    -- Default port is 2222 to connect
    portlabel <- qLabel "Port:  2222"
  
    -- Disable the disconnect button (need to connect to server before we can disconnect)
    setDisabled disconnectB True

    -- Connect buttons to toggle function
    connectSlot connectB "clicked()" connectB "click()" $toggleButton disconnectB
    connectSlot disconnectB "clicked()" disconnectB "click()" $toggleButton connectB

    setAlignment portlabel (fAlignCenter::Alignment)

    -- Second row contains numerous widgets, so lets use a horizontal box layout
    row1layout <- qHBoxLayout ()
    setSpacing row1layout (10::Int)

    -- Add the above widgets to the first row
    addLayout row1layout userlayout
    addWidget row1layout portlabel
    addWidget row1layout connectB
    addWidget row1layout disconnectB

    -- Create layout for row2 (for consistency)
    row2layout <- qHBoxLayout ()
    addWidget row2layout chatDisplay

    -- These two widgets comprise the last line
    chatEntry <- qLineEdit ()
    sendB <- qPushButton "Send"

    row3layout <- qHBoxLayout ()

    addWidget row3layout chatEntry
    addWidget row3layout sendB

    -- Add row1, row2 and row3 to the main layout for the main window
    addLayout vlayout row1layout
    addLayout vlayout row2layout
    addLayout vlayout row3layout

    centralWidget <- qWidget ()
    setLayout centralWidget vlayout

    setCentralWidget mainWindow centralWidget

main :: IO Int
main = do
    qApplication ()

    connectB <- myQPushButton "Connect"
    disconnectB <- myQPushButton "Disconnect"

    username <- qLabel "<None>"
    setFixedWidth username (80::Int)

    -- This is the large main chat display area
    chatDisplay <- qTextEdit ()

    -- Main window for server GUI
    mainWindow <- qMainWindow ()

    initServerGui connectB disconnectB username chatDisplay mainWindow

    showUserName username

    qshow mainWindow ()
    qApplicationExec ()


