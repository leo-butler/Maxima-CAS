# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Menu.tcl,v 1.6 2002/09/14 17:25:34 mikeclarkson Exp $
#

proc pMAXSaveTexToFile {text} {
    set file [tide_savefile [M "Save to a file"] "" *.out]
    if {$file != ""} {
	set contents [$text get 1.0 end]
	set fd [open $file w]
	if {[catch {puts $fd $contents} err]} {
	    tide_failure [M "Error writing to file:\n%s" $err]
	}
	catch {close $fd}
    }
}


proc vMAXAddSystemMenu {fr text} {
    global maxima_priv maxima_default

    set win $fr.textcommands

    # Build a system menubutton
    if {[winfo exists .menu]} {destroy .menu}
    menu .menu
    . configure -menu .menu

    # Add a File menubutton
    set m [menu .menu.file -tearoff 0]
    .menu add cascade -label File -menu $m

    $m add command -underline 0 \
	-accel {Ctrl+b} \
	-label [set label [M {Batch File}]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M "Open a file to Batch"] "" *.mac]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "BATCH(\"$file\")\$\n"
	    }
	}]]
    bind $text <Control-Key-b> $command

    $m add command -underline 11 \
	-accel {Ctrl+o} \
	-label [set label [M {Batch File Silently}]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M "Open a file to BATCHLOAD"] "" *.mac]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "BATCHLOAD(\"$file\")\$\n"
	    }
	}]]
    bind $text <Control-Key-o> $command

    $m add separator
    $m add command -underline 0 \
	-label [set label {Save Expressions to File}] \
	-accel {Ctrl+s} \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_savefile [M "Save to a file"] "" *.bin]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "SAVE(\"$file,ALL\")\$\n"
	    }
	}]]
    bind $text <Control-Key-s> $command


    $m add sep
    $m add command -underline 0 \
	-label {Interrupt} \
	-accel {C-c C-c} \
	-command [list CMinterrupt $text]
    $m add command -underline 0 \
	-label {Restart} \
	-command [list runOneMaxima $text]

    $m add separator
    $m add command -underline 1 \
	-label {Exit} \
	-command [list vMAXExit $text]

    # Add a Edit menubutton
    set m [menu .menu.edit -tearoff 0]
    .menu add cascade -label Edit -menu $m

    $m add command -underline 0 \
	-state disabled \
	-label {Copy} \
	-accel {Ctrl+c} \
	-command [list event generate $text <<Copy>>]
    $m add command -underline 0 \
	-state disabled \
	-label {Cut} \
	-accel {Ctrl+x} \
	-command [list event generate $text <<Cut>>]
    $m add command -underline 0 \
	-state disabled \
	-label {Paste} \
	-accel {Ctrl+v} \
	-command [list event generate $text <<Paste>>]
    $m add separator
    $m add command -underline 0 \
	-label {Previous Input} \
	-accel {Alt-p} \
	-command [list CNpreviousInput $text -1]
    $m add command -underline 0 -label {Clear input} \
	-accel {Ctrl+u} \
	-command [list CNclearinput $text]
    $m add separator
    $m add command -underline 0 -label {Save Console to File} \
	-command [list pMAXSaveTexToFile $maxima_priv(cConsoleText)]

    # Add a Options menubutton
    set m [menu .menu.options -tearoff 0]
    .menu add cascade -label Options -menu $m

    $m add command -underline 0 -label {Toggle Browser Visibility} \
	-command {
	    if { [catch { pack info .browser }] } {
		pack .browser -side bottom
	    } else {
		pack forget .browser
	    }
	}

    $m add separator
    set pm [menu $m.plot]
    $m add cascade -label "Plot Windows" -menu $pm
    foreach elt { embedded separate multiple } {
	$pm add radio -label [string totit $elt] \
	    -variable maxima_default(plotwindow) \
	    -value $elt
    }

    $m add separator
    $m add command -underline 0 \
	-label {Fonts} \
	-command {fontDialog .preferences}
    if {[info commands console] == "console" } {
	$m add sep
	$m add command -underline 0 -label "Show Tcl Console" \
	    -command "console show"
    }

    # Add a Maxima menubutton
    set m [menu .menu.maxima -tearoff 0]
    .menu add cascade -label Maxima -menu $m

    set km [menu $m.kill]
    $m add cascade -label "Clear Memory" -menu $km
    $km add command -label "Kill All" \
	-command [list sendMaxima $text "KILL(ALL)\$\n"]
    $km add separator
    foreach elt {LABELS VALUES FUNCTIONS MACROS ARRAYS \
		     MYOPTIONS PROPS ALIASES RULES GRADEFS \
		     DEPENDENCIES LET_RULE_PACKAGES} {
	$km add command -label "Kill [string totit $elt]" \
	    -command [list sendMaxima $text "KILL($elt)\$\n"]
    }
    $m add separator
    set dir $maxima_priv(pTestsDir)
    if {[file isdir $dir]} {
	set state normal
    } else {
	set state disabled
    }
    $m add command -underline 0 \
	-state $state \
	-label {Run Tests} \
	-command "sendMaxima $text {:lisp (progn (si::chdir \"$dir\")(load \"tests.lisp\"))\n}"


    # Add a Help menubutton
    set m [menu .menu.help -tearoff 0]
    .menu add cascade -label Help -menu $m

    set file $maxima_priv(pReferenceToc)
    if {[file isfile $file]} {
	set state normal
    } else {
	set state disabled
    }
    $m add command -underline 7 -label {Maxima Help} \
	-state $state \
	-command "OpenMathOpenUrl \"file:/$file\""
    set browse {exec}
    global tcl_platform
    if {$tcl_platform(platform) == "windows"} {
	lappend browse start
} else {
	# FIXME: get a browser object
	lappend browse [auto_execok netscape]
    }
    $m add sep
    $m add command -underline 0 -label {Maxima Homepage} \
	-command [list eval $browse http://maxima.sourceforge.net &]
    $m add command -underline 0 -label {Project Page} \
	-command [list eval $browse http://sourceforge.net/projects/maxima &]
    $m add command -underline 0 -label {Bug Reports} \
	-command [list eval $browse \
		      {http://sourceforge.net/tracker/?group_id=4933&atid=104933} &]

    rename vMAXAddSystemMenu ""
    # vMAXSystemMenuHandlers $text $event

    # Backwards compatability
    return $win
}
