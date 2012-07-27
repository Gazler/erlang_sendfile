-module(message_client).
-export([logon/1, logoff/0, message/2, client/2, user_list/0, test/0, test2/0, send_file/2]).

server_node() ->
    'messenger@gazler-desktop'.

%%% The client process which runs on each server node
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        user_list ->
            {messenger, Server_Node} ! {self(), user_list};
        {send_file, ToName, FileLocation} ->
            {ok, File} = file:read_file(FileLocation),
            FileName = hd(lists:reverse(string:tokens(FileLocation, "/"))),
            {messenger, Server_Node} ! {self(), send_file, ToName, File, FileName};

        {file_received, Name, File, FileName} ->
            Size = size(File),
            {ok, FileHandler} = file:open(string:concat("received/", FileName), [write]),
            case file:write(FileHandler, File) of
              ok ->
                io:format("file received: ~p~n", [FileName]);
              {error, Reason} ->
                io:format("Can't write file, ~p~n", [Reason])
            end;
        {display_user_list, User_List} ->
            io:format("~n"),
            print_user_list(User_List);
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

print_user_list([]) ->
  io:format("No Users~n");
print_user_list([H|[]]) ->
  {_, Name} = H,
  io:format("~p~n", [Name]);
print_user_list([H|T]) ->
  {_, Name} = H,
  io:format("~p~n", [Name]),
  print_user_list(T).

test() ->
  logon(gazler).

test2() ->
  logon(gazler2),
  user_list(),
  send_file(gazler, "/home/gazler/stream.txt").

%%% User Commands
logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, 
                     spawn(message_client, client, [server_node(), Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ ->
             mess_client ! {message_to, ToName, Message},
             ok
    end.

send_file(ToName, FileLocation) ->
  case whereis(mess_client) of
    undefined ->
      not_logged_on;
    _ ->
      mess_client ! {send_file, ToName, FileLocation}
  end.
    

user_list() ->
  case whereis(mess_client) of
    undefined ->
      not_logged_on;
    _ ->
      mess_client ! user_list
  end.

%%% wait for a response from the server
await_result() ->
    receive
        {messenger, stop, Why} -> % Stop the client 
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->  % Normal response
            io:format("~p~n", [What])
    end.
