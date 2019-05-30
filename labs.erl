-module(labs).

-export([init/1, add_lab/2, add_student/3, remove_student/1, remove_lab/1]).

-record(labs, {date, teacher}).
-record(students, {firstname, surname, lab_date, points = []}).

init(Nodes) -> 
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, mnesia, start, []),  
    mnesia:create_table(labs, 
                        [{attributes, record_info(fields, labs)},
                        {disc_copies, Nodes},
                        {index, [#labs.date]}]),
    mnesia:create_table(students, 
                        [{attributes, record_info(fields, students)},
                        {disc_copies, Nodes},
                        {index, [#students.surname]}]),
    rpc:multicall(Nodes, application, stop, []).

add_lab(Date, Teacher) ->
    Lab = #labs{
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
    F = fun() -> case mnesia:index_read(labs, Date, date) of
                    []      -> {error, no_such_lab};
                    [Lab]   -> mnesia:write(Student)
                 end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

remove_student(Surname) -> 
    F = fun() -> case mnesia:index_read(students, Surname, surname) of
                     []         -> {error, no_such_student};
                     [Student]  -> mnesia:delete({students, Student#students.surname})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

remove_lab(Date) ->
    F = fun() -> case mnesia:index_read(labs, Date, date) of
                     []         -> {error, no_such_lab};
                     [Lab]      -> mnesia:delete({labs, Lab#labs.date})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

add_point_to_student(Value, Surname) ->
    F = fun() -> case mnesia:index_read(students, Surname, surname) of
                     []         -> {error, no_such_student};
                     [Student]  -> mnesia:write( students#Student{points

