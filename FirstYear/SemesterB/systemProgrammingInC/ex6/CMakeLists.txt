cmake_minimum_required(VERSION 3.25)
project(ex6 C)

set(CMAKE_C_STANDARD 11)

add_executable(ex6 summarizeJournal.c journalLog.c)
