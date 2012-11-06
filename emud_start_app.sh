#!/bin/bash
erl -pa ./ebin  -sname emud -setcookie 'emud' -kernel error_logger '{file,"/tmp/log"}' -s emud start -s mymap init -s -config default.config 
