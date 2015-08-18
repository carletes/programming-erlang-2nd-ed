-module(afile_client).
-export([ls/1, get_file/2, put_file/3]).

ls(FileServer) ->
    FileServer ! {self(), list_dir},
    receive
        {_, Files} ->
            Files
    end.

get_file(FileServer, File) ->
    FileServer ! {self(), get_file, File},
    receive
        {_, Content} ->
            Content
    end.

put_file(FileServer, File, Content) ->
    FileServer ! {self(), put_file, File, Content},
    receive
        {_, Result} ->
            Result
    end.
