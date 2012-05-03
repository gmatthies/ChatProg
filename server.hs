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

-- I admit, this is an ugly approach
initServerGui startB stopB sendB chatEntry username chatDisplay mainWindow = do
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
  
    -- Disable the stop button (need to start server before we can disconnect)
    setDisabled stopB True

    setAlignment portlabel (fAlignCenter::Alignment)

    -- Second row contains numerous widgets, so lets use a horizontal box layout
    row1layout <- qHBoxLayout ()
    setSpacing row1layout (10::Int)

    -- Add the above widgets to the first row
    addLayout row1layout userlayout
    addWidget row1layout portlabel
    addWidget row1layout startB
    addWidget row1layout stopB

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

-- Processing for when a new client connects
handleNewClient :: QTcpSocket () -> QTextEdit () -> QTcpServer () -> IO ()
handleNewClient socket chatDisplay server = do
    socket <- nextPendingConnection server ()
    append chatDisplay "Client has connected!"
    connectSlot socket "readyRead()" socket "handleReadSocket()" $ handleReadSocket chatDisplay

-- Processing to read from the socket and add contents to the chat display
handleReadSocket :: QTextEdit () -> QTcpSocket () -> IO ()
handleReadSocket chatDisplay socket = do
    contents <- readAll socket ()
    append chatDisplay contents

-- Sends a message to the connected client (button
sendMessage :: QTcpSocket () -> QTextEdit () -> String -> QLineEdit () -> MyQPushButton -> IO () 
sendMessage socket chatDisplay prompt chatEntry this = do
    linetext <- text chatEntry ()
    let message = prompt ++ linetext
    append chatDisplay message
    write socket message
    clear chatEntry ()

-- Begins the server
-- Due to not knowing how to use findChild, I am just passing each important widget around.
-- Not an ideal solution, but I am not quite certain what to do other than this.
-- Might be able to make a list and pass the list around...or at least a tuple
startServer :: MyQPushButton -> MyQPushButton -> String -> QLineEdit () -> QTextEdit () -> QTcpServer () -> QTcpSocket () -> MyQPushButton -> IO ()
startServer stopB sendB prompt chatE chatD server socket this = do
    -- Set up host address and listen on port 2222
    listenAddress <- qHostAddress hostAddress
    listen server (listenAddress, portNum)

    -- Start off by disconnecting the slots 
    disconnectSlot server "newConnection()"
    disconnectSlot chatE "returnPressed()"
    disconnectSlot sendB "clicked()"

    listenbool <- isListening server ()
    if listenbool /= True
        then do
            append chatD "Error: Could not start server!"
        else do
            setEnabled this False
            setEnabled stopB True  
            append chatD $ "SERVER: " ++ hostAddress
            append chatD $ "PORT:       2222"
            append chatD $ "Server is running..."

            -- Now, we need talo connect newConnection slot to a useful function
{--
      QObject.connect(self.tcpServer, SIGNAL("newConnection()"), self.newConnectionArrives )
      QObject.connect(self.lineedit, SIGNAL("returnPressed()"), self.lineeditReturnPressed )
--}
            connectSlot server "newConnection()" server "handleNewClient()" $ handleNewClient socket chatD
            connectSlot chatE "returnPressed()" sendB "sendMessage()" $ sendMessage socket chatD prompt chatE
            connectSlot sendB "clicked()" sendB "sendMessage()" $ sendMessage socket chatD prompt chatE

--startServer stopB sendB prompt chatE chatD server socket this
stopServer :: MyQPushButton -> MyQPushButton -> QLineEdit () -> QTextEdit () -> QTcpServer () -> QTcpSocket () -> MyQPushButton -> IO ()
stopServer startB sendB chatE chatD server socket this = do
    setEnabled startB True
    setEnabled this False

    disconnectSlot sendB "clicked()"
    disconnectSlot chatE "returnPressed()"
    disconnectSlot server "newConnection()"
    append chatD "Server is disconnected..."

main :: IO Int
main = do
    qApplication ()

    startB <- myQPushButton "Start"
    stopB <- myQPushButton "Stop"

    -- These two widgets comprise the last line and the send message button
    chatEntry <- qLineEdit ()
    sendB <- myQPushButton "Send"

    username <- qLabel "<None>"
    setFixedWidth username (80::Int)

    -- This is the large main chat display area
    chatDisplay <- qTextEdit ()
    setReadOnly chatDisplay True

    -- Main window for server GUI
    mainWindow <- qMainWindow ()

    initServerGui startB stopB sendB chatEntry username chatDisplay mainWindow

    showUserName username

    -- Get username from the label and build up a client promp string
    -- This string will be appended with the message to send and sent to the server
    usernameStr <- text username () 
    let serverPrompt = usernameStr ++ " says: " 

    -- Create TCP server and TCP socket
    tcpServer <- qTcpServer ()
    tcpSocket <- qTcpSocket ()

    -- Connect start and stop buttons to appropriate functions
    connectSlot startB "clicked()" startB "startServer()" $startServer stopB sendB serverPrompt chatEntry chatDisplay tcpServer tcpSocket

    connectSlot stopB "clicked()" stopB "stopServer()" $ stopServer startB sendB chatEntry chatDisplay tcpServer tcpSocket


    qshow mainWindow ()
    qApplicationExec ()


