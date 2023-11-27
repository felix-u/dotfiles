@echo off

set agenda_file="C:\Users\Felix\Desktop\uni\2023\autumn\agenda"

(nota %agenda_file% --line-num -bs ascending -n task --not-tagged & ^
    nota %agenda_file% --line-num -un task --not-tagged) | less