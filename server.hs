module Main where

import Data.IORef
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

type MyQWidget = QWidgetSc (CMyQWidget)
data CMyQWidget = CMyQWidget

myQWidget :: QWidget () -> IO (MyQWidget)
myQWidget mw = qSubClass $ qCast_QWidget mw

type MyQTcpSocket = QTcpSocketSc (CMyQTcpSocket)
data CMyQTcpSocket = CMyQTcpSocket

myQTcpSocket :: (QTcpServer () -> () -> IO (QTcpSocket t0)) -> QTcpServer () -> IO (MyQTcpSocket)
myQTcpSocket gf gp = qSubClass $ gf gp ()

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton b = qSubClass $ qPushButton b

initRow1 :: IO (QHBoxLayout ())
initRow1 = do
    -- First row holds username information
    portlabel <- qLabel "Port:"
    setFixedWidth portlabel (50::Int)

    -- Use a spin box for the port number, restricting values to valid numbers
    -- Default port number is 2222
    portSpin  <- qSpinBox ()
    setMinimum portSpin (1::Int)
    setMaximum portSpin (65535::Int)
    setValue portSpin (2222::Int)  

    startB <- myQPushButton "Start"
    stopB <- myQPushButton "Stop"
    setDisabled stopB True

    row1layoutL <- qHBoxLayout ()
    row1layoutR <- qHBoxLayout ()

    addWidget row1layoutL portlabel
    addWidget row1layoutL portSpin
    setAlignment row1layoutL (fAlignLeft::Alignment)
    
    addWidget row1layoutR startB
    addWidget row1layoutR stopB 
    setAlignment row1layoutR (fAlignRight::Alignment)


    row1 <- qHBoxLayout ()
    addLayout row1 row1layoutL
    addLayout row1 row1layoutR
    return row1

main :: IO Int
main = do
    qApplication ()

    -- Main vertical box layout
    vlayout <- qVBoxLayout ()
  
    -- Since row one is busy, move the creation into a function to clean up main
    row1layout <- initRow1

    {--
    What I need to do is probably have an init_gui function to handle a huge chunk of initializing things
    I need to make sure I can pass in the tree widget to the startClicked and stopClicked functions which still
    to be made. In addition, I might get rid of the port spin box to just use a static value, should make things
    easier. I might also just see about getting a server and client working such that only two people can connect.
    NOTE: Email teacher about this, might be sufficient...and I could use the simplicity (if so, make it so that
    both the server and client can be given names - Like Greg and Kathy)

    NOTE2: Along with this idea, might make sense to try and solve this "on paper" first

    --}

    clientBox <- qGroupBox "Connected users"

    headerItem <- qTreeWidgetItem ()
    setText headerItem (0::Int, "Host")
    setText headerItem (1::Int, "Port")
    setText headerItem (2::Int, "last")

    treeWidget <- qTreeWidget () 
    setHeaderItem treeWidget headerItem   

    treeLayout <- qVBoxLayout ()    
    addWidget treeLayout treeWidget

    setLayout clientBox treeLayout    

    addLayout vlayout row1layout
    addWidget vlayout clientBox

    centralWidget <- qWidget ()
    setLayout centralWidget vlayout
    mainWindow <-qMainWindow ()

    setCentralWidget mainWindow centralWidget

    qshow mainWindow ()
    qApplicationExec ()


