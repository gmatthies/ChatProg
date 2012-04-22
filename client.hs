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

main :: IO Int
main = do
    qApplication ()

    -- Main vertical box layout
    vlayout <- qVBoxLayout ()

    -- First row contains numerous widgets, so lets use a horizontal box layout
    row1layout <- qHBoxLayout ()

    hostlabel <- qLabel "Host"
    hostline  <- qLineEdit ()
    portlabel <- qLabel "Port"

    portSpin  <- qSpinBox ()
    setMinimum portSpin (1::Int)
    setMaximum portSpin (65535::Int)
    setValue portSpin (2222::Int)  
  
    connectB <- qPushButton "Connect"
    disconnectB <- qPushButton "Disconnect"

    setAlignment hostlabel (fAlignCenter::Alignment)
    setAlignment portlabel (fAlignCenter::Alignment)

    -- Add the above widgets to the first row
    addWidget row1layout hostlabel
    addWidget row1layout hostline
    addWidget row1layout portlabel
    addWidget row1layout portSpin
    addWidget row1layout connectB
    addWidget row1layout disconnectB

    -- This is the large main chat display area
    chatDisplay <- qTextEdit ()

    -- These two widgets comprise the last line
    chatEntry <- qLineEdit ()
    sendB <- qPushButton "Send"

    row2layout <- qHBoxLayout ()

    addWidget row2layout chatEntry
    addWidget row2layout sendB

    addLayout vlayout row1layout
    addWidget vlayout chatDisplay
    addLayout vlayout row2layout

    centralWidget <- qWidget ()
    setLayout centralWidget vlayout
    mainWindow <-qMainWindow ()

    setCentralWidget mainWindow centralWidget

    qshow mainWindow ()
    qApplicationExec ()


