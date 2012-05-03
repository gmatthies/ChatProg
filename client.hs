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

hostAddress = "127.0.0.1"
portNum = (2222::Int)

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

    -- Default port is 2222 to connect
    portlabel <- qLabel "Port:  2222"
  
    -- Disable the disconnect button (need to connect to server before we can disconnect)
    setDisabled disconnectB True

    -- Connect buttons to toggle function
    -- connectSlot connectB "clicked()" connectB "click()" $toggleButton disconnectB
    -- connectSlot disconnectB "clicked()" disconnectB "click()" $toggleButton connectB

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

-- Sends a message to the connected client (button
sendMessage :: QTcpSocket () -> QTextEdit () -> String -> QLineEdit () -> MyQPushButton -> IO () 
sendMessage socket chatDisplay prompt chatEntry this = do
    linetext <- text chatEntry ()
    let message = prompt ++ linetext
    append chatDisplay message
    write socket message
    clear chatEntry ()

-- Connects client to the server
connectToServer :: MyQPushButton -> MyQPushButton -> String -> QLineEdit () -> QTcpSocket () -> QTextEdit () -> MyQPushButton -> IO ()
connectToServer disconnectB sendB prompt chatE socket chatD this = do
    setEnabled disconnectB True
    setEnabled this False

    -- I don't think this is working
    connectToHost socket (hostAddress, (1543::Int))

    valid <- qisValid socket ()

    putStrLn valid

    if valid == False
        then do
            append chatD "Error: Cannot connect to server!"
        else do
            append chatD "Connected to server!"
            connectSlot chatE "returnPressed()" sendB "sendMessage()" $ sendMessage socket chatD prompt chatE
            connectSlot sendB "clicked()" sendB "sendMessage()" $ sendMessage socket chatD prompt chatE
            connectSlot socket "connected()" socket "socketConnected()" $ socketConnected chatD
            connectSlot socket "readyRead()" socket "handleReadSocket()" $ handleReadSocket chatD 


-- Processing to read from the socket and add contents to the chat display
handleReadSocket :: QTextEdit () -> QTcpSocket () -> IO ()
handleReadSocket chatDisplay socket = do
    contents <- readAll socket ()
    append chatDisplay contents

socketConnected :: QTextEdit () -> QTcpSocket () -> IO ()
socketConnected chatDisplay socket = do
    let message = "Successfully connected to server!"
    append chatDisplay message
    write socket message
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

    -- Get username from the label and build up a client promp string
    -- This string will be appended with the message to send and sent to the server
    usernameStr <- text username () 
    let clientPrompt = usernameStr ++ " says: " 

    tcpSocket <- qTcpSocket ()

    connectSlot connectB "clicked()" connectB "connectToServer()" $ connectToServer disconnectB sendB clientPrompt chatEntry tcpSocket chatDisplay

    qshow mainWindow ()
    qApplicationExec ()

