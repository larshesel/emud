#!/bin/bash

erl -pa ./ebin -noshell  -sname emud -setcookie 'emud' -s emud start -s mymap init
