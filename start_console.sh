#!/bin/bash

erl -pa ./ebin -noshell -s mymap init -s init stop -sname emud -setcookie 'emud'
