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

hostAddress = "0.0.0.0"
portNum = (2222::Int)

-- Connects client to the server
connectToServer :: MyQPushButton -> MyQPushButton -> String -> QLineEdit () -> QTcpSocket () -> QTextEdit () -> MyQPushButton -> IO ()
connectToServer disconnectB sendB prompt chatE socket chatD this = do
    setEnabled disconnectB True
    setEnabled this False

    -- I don't think this is working
    connectToHost socket (hostAddress, portNum)

    valid <- qisValid socket ()

    print valid

    if valid == False
        then do
            append chatD "Error: Cannot connect to server!"
        else do
            sockstate <- state socket ()
            print sockstate
            append chatD "Connected to server!"
            connectSlot chatE "returnPressed()" sendB "handleSendMessage()" $ handleSendMessage socket chatD prompt chatE
            connectSlot sendB "clicked()" sendB "handleSendMessage()" $ handleSendMessage socket chatD prompt chatE
            connectSlot socket "connected()" socket "handleSocketConnected()" $ handleSocketConnected chatD
            connectSlot socket "readyRead()" socket "handleReadSocket()" $ handleReadSocket chatD 

-- Processing to read from the socket and add contents to the chat display
handleReadSocket :: QTextEdit () -> QTcpSocket () -> IO ()
handleReadSocket chatDisplay socket = do
    contents <- readAll socket ()
    append chatDisplay contents

-- Sends a message to the connected client (button
handleSendMessage :: QTcpSocket () -> QTextEdit () -> String -> QLineEdit () -> MyQPushButton -> IO () 
handleSendMessage socket chatDisplay prompt chatEntry this = do
    linetext <- text chatEntry ()
    -- Don't send empty strings
    if length linetext > 0
        then do
            let message = prompt ++ linetext
            append chatDisplay message
            write socket message
            clear chatEntry ()
        else do
            return ()

handleSocketConnected :: QTextEdit () -> QTcpSocket () -> IO ()
handleSocketConnected chatDisplay socket = do
    let message = "Successfully connected to server!"
    append chatDisplay message
    write socket message
    return ()

initClientGui connectB disconnectB sendB chatEntry username chatDisplay mainWindow = do
    -- Main vertical box layout
    vlayout <- qVBoxLayout ()

    -- First row holds username information
    namelabel <- qLabel "Username:"
    setFixedWidth namelabel (80::Int)

    userlayout <- qHBoxLayout ()
    addWidget userlayout namelabel
    addWidget userlayout username
    setAlignment userlayout (fAlignLeft::Alignment)

    portlabel <- qLabel ("Port:  " ++ (show portNum))
    setAlignment portlabel (fAlignCenter::Alignment)
  
    -- Disable the disconnect button (need to connect to server before we can disconnect)
    setDisabled disconnectB True

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

showUserName namelabel = do
    dlayout <- qVBoxLayout ()
    dialog <- qDialog ()

    nameEntryLabel <- qLabel "Enter name"
    nameEntry <- qLineEdit ()
    doneB <- myQPushButton "Done"

    addWidget dlayout nameEntryLabel
    addWidget dlayout nameEntry
    addWidget dlayout doneB

    setLayout dialog dlayout

    connectSlot doneB "clicked()" doneB "updateName()" $ updateName nameEntry namelabel dialog

    exec dialog ()

updateName :: QLineEdit () -> QLabel () -> QDialog () -> MyQPushButton -> IO ()
updateName nameEntry namelabel dialog this = do
    theName <- text nameEntry ()
    setText namelabel theName
    done dialog (1::Int)
    return ()    
    
main :: IO Int
main = do
    qApplication ()

    connectB <- myQPushButton "Connect"
    disconnectB <- myQPushButton "Disconnect"

    username <- qLabel "<None>"
    setFixedWidth username (80::Int)

    -- These two widgets comprise the last line and the send message button
    chatEntry <- qLineEdit ()
    sendB <- myQPushButton "Send"

    -- This is the large main chat display area
    chatDisplay <- qTextEdit ()
    setReadOnly chatDisplay True

    -- Main window for server GUI
    mainWindow <- qMainWindow ()

    initClientGui connectB disconnectB sendB chatEntry username chatDisplay mainWindow

    showUserName username

    -- Get username from the label and build up a client prompt string
    -- This string will be appended with the message to send and sent to the server
    usernameStr <- text username () 
    let clientPrompt = usernameStr ++ " says: " 

    tcpSocket <- qTcpSocket ()

    connectSlot connectB "clicked()" connectB "connectToServer()" $ connectToServer disconnectB sendB clientPrompt chatEntry tcpSocket chatDisplay

    qshow mainWindow ()
    qApplicationExec ()

