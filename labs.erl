-module(labs).

-export([
         init/1, delete/1, add_lab/2, add_student/3, 
         remove_student/1, remove_lab/1, add_point_to_student/2
        ]).

-record(laboratories, {date, teacher}).
-record(students, {surname, firstname, lab_date, points = []}).

delete(Nodes) -> mnesia:delete_table(laboratories),
                 mnesia:delete_table(students),
                 mnesia:stop(),
                 mnesia:delete_schema(Nodes).

init(Nodes) -> 
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, mnesia, start, []),  
    {atomic, ok} = mnesia:create_table(laboratories, 
                        [{attributes, record_info(fields, laboratories)},
                        {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(students, 
                        [{attributes, record_info(fields, students)},
                        {disc_copies, Nodes}
                        ]).

add_lab(Date, Teacher) ->
    Lab = #laboratories{
             date = Date, 
             teacher = Teacher
            },
    F = fun() -> mnesia:write(Lab) end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

add_student(FirstName, Surname, Date) -> 
    Student = #students{
                 firstname = FirstName,
                 surname = Surname, 
                 lab_date = Date
                },
    F = fun() -> case mnesia:read({laboratories, Date}) of
                    []      -> {error, no_such_lab};
                    [_]     -> mnesia:write(Student)
                 end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

remove_student(Surname) -> 
    F = fun() -> case mnesia:read({students, Surname}) of
                     []         -> {error, no_such_student};
                     [Student]  -> mnesia:delete({students, Student#students.surname})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

remove_lab(Date) ->
    F = fun() -> case mnesia:read({laboratories, Date}) of
                     []         -> {error, no_such_lab};
                     [Lab]      -> mnesia:delete({laboratories, Lab#laboratories.date})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

add_point_to_student(Value, Surname) ->
    F = fun() -> case mnesia:read({students, Surname}) of
                     []         -> {error, no_such_student};
                     [Student]  -> mnesia:delete({students, Surname}),
                                   mnesia:write(Student#students{points = [Value | Student#students.points]})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result. 

%TO DO
%
%select labs - CZAJKA ZA CO CI PŁACE!!!
%
%select students - CZAJKA ZA CO CI PŁACE!!!
%
%add observer node
%
%remove observer node
