#!/bin/bash

erl -pa ./ebin -noshell  -sname emud -setcookie 'emud' -kernel error_logger '{file,"/tmp/log"}' -s emud start -s mymap init -s emud_console login -config default.config 
