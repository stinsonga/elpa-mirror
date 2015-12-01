;;; transcribe.el --- package for audio transcriptions

;; Copyright 2014-2015  Free Software Foundation, Inc.

;; Author: David Gonzalez Gandara <dggandara@member.fsf.org>
;; Version: 0.5.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; REQUIRES:
;; -----------------------------
;; In order to use the most important functions of transcribe, you need to install emms and mpg321.
;;
;; USAGE:
;; -------------------------
;; Transcribe is a tool to make audio transcriptions easy. It allows the transcriber to control the audio easily while typing, as well as automate the insertion of xml tags, in case the transcription protocol include them.
;; 
;;  AUDIO COMMANDS
;; ------------------------------
;;     C-x C-p --------> Play audio file. You will be prompted for the name of the file. The recommended format is mp2.
;;     <f5> -----------> Pause or play audio.
;;     C-x <right> ----> seek audio 10 seconds forward.
;;     C-x <left> ----->seek audio 10 seconds backward.
;;     <f8> -----------> seek interactively: positive seconds go forward and negative seconds go backward
;;
;;  XML TAGGING COMMANDS
;; --------------------------------------------------
;;     C-x C-n --> Create new episode structure. This is useful in case your xml file structure requires it. You can customize the text inserted manipulating the realted function.
;;     <f6> -----> Interactively insert new tag. You will be prompted for the content of the tag. The starting tag and the end tag will be inserted automatically and the cursor placed in the proper place to type.
;;
;;
;;
;; SPECIFIC COMMANDS I USE, THAT YOU MAY FIND USEFUL
;; ------------------------------------------------
;;     C-x C-a ------> This runs an external discourse analysis tool. It defaults to my own script analyze_episodes2.py, but you can customise the command to launch any other.
;;     <f11> --------> Customised tag 1. Edit the function to adapt to your needs.
;;     <f12> --------> Customised tag 2. Edit the function to adapt to your needs.
;;     <f7> ---------> Break tag. This command "breaks" a tag in two, that is it inserts an ending tag and then a starting tag. Edit the function to suit your needs. It is useful if you are segmenting discourse into tags and then you decide the segmentation was not correct.
;;     <f4> ---------> Insert atributes. This function insert custom xml attributes. Edit the function to suit you needs.

;;; Code:

(if t (require 'emms-setup))
;(require 'emms-player-mpd)
;(setq emms-player-mpd-server-name "localhost")
;(setq emms-player-mpd-server-port "6600")

(emms-standard)
(emms-default-players)
(if t (require 'emms-player-mpg321-remote))
(defvar emms-player-list)
(push 'emms-player-mpg321-remote emms-player-list)

(if t (require 'emms-mode-line))
(emms-mode-line 1)
(if t (require 'emms-playing-time))
(emms-playing-time 1)


(defun transcribe-analyze-episode (episode person)
  (interactive "sepisode: \nsperson:")
  (shell-command (concat (expand-file-name  "analyze_episodes2.py") " -e " episode " -p " person " -i " buffer-file-name )))

(defun transcribe-define-xml-tag (xmltag)
  (interactive "stag:")
  (insert (format "<%s></%s>" xmltag xmltag))
  (backward-char 3)
  (backward-char (string-width xmltag)))

(defun transcribe-xml-tag-l1 ()
  (interactive)
  (insert "<l1></l1>")
  (backward-char 3)
  (backward-char 2))

(defun transcribe-xml-tag-l2 ()
  (interactive)
  (insert "<l2 clauses=\"1\" errors=\"0\"></l2>")
  (backward-char 3)
  (backward-char 2))

(fset 'transcribe-xml-tag-l2-break "</l2><l2 clauses=\"1\" errors=\"0\">")
(fset 'transcribe-set-attributes "clauses=\"1\" errors=\"0\"")

(defun transcribe-display-audio-info ()
  (interactive)
  (emms-player-mpg321-remote-proc)
  (shell-command "/usr/bin/mpg321 -R - &"))


(fset 'NewEpisode
      "<episode>\n<number>DATE-NUMBER</number>\n<duration></duration>\n<comment></comment>\n<subject>Subject (level)</subject>\n<task>\n\t<role>low or high</role>\n<context>low or high</context>\n<demand>low or high</demand>\r</task>\n<auxiliar>Yes/no</auxiliar>\n<transcription>\n</transcription>\n</episode>")

(define-minor-mode transcribe-mode
 "Toggle transcribe-mode"
  nil
  " Transcribe"
  '(([?\C-x ?\C-p] . emms-play-file)
    ([?\C-x ?\C-a] . transcribe-analyze-episode)
    ([?\C-x ?\C-n] . 'NewEpisode)
    ([?\C-x down] . emms-stop)
    ([?\C-x right] . emms-seek-forward)
    ([?\C-x left] . emms-seek-backward)
    ([f5] . emms-pause)
    ([f6] . transcribe-define-xml-tag)
    ([f7] . 'transcribe-xml-tag-l2-break)
    ([f8] . emms-seek)
    ([f4] . 'transcribe-set-atributes)
    ([f11] . transcribe-xml-tag-l1)
    ([f12] . transcribe-xml-tag-l2))
)

;;;; ChangeLog:

;; 2015-11-30  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Added minor-mode function as suggested
;; 
;; 2015-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* transcribe.el: Add `provide' statement
;; 
;; 2015-11-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* transcribe.el: Fix up formatting and copyright
;; 
;; 2015-11-29  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Added some usage information
;; 
;; 2015-11-29  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	Package transcribe added
;; 


(provide 'transcribe)

;;; transcribe.el ends here
