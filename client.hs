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

main :: IO Int
main = do
    qApplication ()

    -- Main vertical box layout
    vlayout <- qVBoxLayout ()

    -- First row holds username information
    namelabel <- qLabel "Username:"
    setFixedWidth namelabel (100::Int)
    username <- qLabel "<None>"

    userlayout <- qHBoxLayout ()

    addWidget userlayout namelabel
    addWidget userlayout username

    -- Second row contains numerous widgets, so lets use a horizontal box layout
    row1layout <- qHBoxLayout ()

    hostlabel <- qLabel "Host"
    hostline  <- qLineEdit ()
    portlabel <- qLabel "Port"

    -- Use a spin box for the port number, restricting values to valid numbers
    -- Default port number is 2222
    portSpin  <- qSpinBox ()
    setMinimum portSpin (1::Int)
    setMaximum portSpin (65535::Int)
    setValue portSpin (2222::Int)  
  
    connectB <- myQPushButton "Connect"
    disconnectB <- myQPushButton "Disconnect"
    setDisabled disconnectB True

    -- Connect buttons to toggle function
    connectSlot connectB "clicked()" connectB "click()" $toggleButton disconnectB
    connectSlot disconnectB "clicked()" disconnectB "click()" $toggleButton connectB

    setAlignment hostlabel (fAlignCenter::Alignment)
    setAlignment portlabel (fAlignCenter::Alignment)

    -- Add the above widgets to the first row
    addWidget row1layout hostlabel
    addWidget row1layout hostline
    addWidget row1layout portlabel
    addWidget row1layout portSpin
    addWidget row1layout connectB
    addWidget row1layout disconnectB


    -- This is the large main chat display area (second row)
    chatDisplay <- qTextEdit ()

    -- These two widgets comprise the last line
    chatEntry <- qLineEdit ()
    sendB <- qPushButton "Send"

    row2layout <- qHBoxLayout ()

    addWidget row2layout chatEntry
    addWidget row2layout sendB

    addLayout vlayout userlayout
    addLayout vlayout row1layout
    addWidget vlayout chatDisplay
    addLayout vlayout row2layout

    centralWidget <- qWidget ()
    setLayout centralWidget vlayout
    mainWindow <-qMainWindow ()

    setCentralWidget mainWindow centralWidget

    showUserName username

    qshow mainWindow ()
    qApplicationExec ()


