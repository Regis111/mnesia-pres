-module(labs).

-export([init/1, delete/0]).
-export([remove_student/1, remove_lab/1]).
-export([add_lab/2, add_student/3, add_point_to_student/2]).
-export([select_lab_with_date/1, select_student_with_surname/1, 
         select_all_students/0, select_all_labs/0]).

-record(laboratories, {date, teacher}).
-record(students, {surname, firstname, lab_date, points = []}).

-include_lib("stdlib/include/qlc.hrl").

delete() -> mnesia:delete_table(laboratories),
            mnesia:delete_table(students).

init(Nodes) -> 
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
                    []      -> mnesia:abort(no_such_lab);
                    [_]     -> mnesia:write(Student)
                 end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

remove_student(Surname) -> 
    F = fun() -> case mnesia:read({students, Surname}) of
                     []         -> mnesia:abort(no_such_student);
                     [Student]  -> mnesia:delete({students, Student#students.surname})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

remove_lab(Date) ->
    F = fun() -> case mnesia:read({laboratories, Date}) of
                     []         -> mnesia:abort(no_such_lab);
                     [Lab]      -> mnesia:delete({laboratories, Lab#laboratories.date})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

add_point_to_student(Value, Surname) ->
    F = fun() -> case mnesia:read({students, Surname}) of
                     []         -> mnesia:abort(no_such_student);
                     [Student]  -> mnesia:delete({students, Surname}),
                                   mnesia:write(Student#students{points = [Value | Student#students.points]})
                 end
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result. 

select_lab_with_date(Date) ->
    F = fun() ->
                [Lab] = mnesia:read(labs, Date),
                Lab
        end,
    mnesia:transaction(F).

select_student_with_surname(Surname) ->
    F = fun() -> 
                [Student] = mnesia:read(students, Surname),
                Student
        end,
    mnesia:transaction(F).

select_all_students() ->
    F = fun() ->
                qlc:eval(qlc:q(
                           [{F,L,D,P} || 
                            #students{surname = L,firstname = F, lab_date = D, points = P} 
                            <- mnesia:table(students)]))
        end,
    mnesia:transaction(F).

select_all_labs() ->
    F = fun() ->
                qlc:eval(qlc:q(
                           [{D, T} || 
                            #laboratories{date = D, teacher = T} 
                            <- mnesia:table(laboratories)]))
        end,
    mnesia:transaction(F).

add_node(Node) -> mnesia:change_config(extra_db_nodes, [Node]),
                  mnesia:change_table_copy_type(schema, Node, disc_copies),
                  [{Tb, mnesia:add_table_copy(Tb, node(), Type)} || {Tb, [{'a@node', Type}]} 
                    <- [{T, mnesia:table_info(T, where_to_commit)} || T <- mnesia:system_info(tables)]].

%%TO DO
