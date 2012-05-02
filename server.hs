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

main :: IO Int
main = do
    qApplication ()

    -- Main vertical box layout
    vlayout <- qVBoxLayout ()

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

    row1layoutOuter <- qHBoxLayout ()
    addLayout row1layoutOuter row1layoutL
    addLayout row1layoutOuter row1layoutR
    setAlignment row1layoutOuter (fAlignTop::Alignment)

    clientBox <-qGroupBox "Connected users"

    headerItem <- qTreeWidgetItem ()
    setText headerItem (0::Int, "Host")
    setText headerItem (1::Int, "Port")
    setText headerItem (2::Int, "last")

    treeWidget <- qTreeWidget () 
    setHeaderItem treeWidget headerItem   

    treeLayout <- qVBoxLayout ()    
    addWidget treeLayout treeWidget

    setLayout clientBox treeLayout    

    addLayout vlayout row1layoutOuter
    addWidget vlayout clientBox

    centralWidget <- qWidget ()
    setLayout centralWidget vlayout
    mainWindow <-qMainWindow ()

    setCentralWidget mainWindow centralWidget

    qshow mainWindow ()
    qApplicationExec ()


