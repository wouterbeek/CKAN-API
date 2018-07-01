:- module(
  ckan_export,
  [
    ckan_export/1, % +Uri
    ckan_export/2  % +Uri, -Directory
  ]
).

/** <module> CKAN export

Exports a CKAN site to a collection of JSON files.

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(http/json)).
:- use_module(library(zlib)).

:- use_module(library(date_time)).
:- use_module(library(file_ext)).
:- use_module(library(http/ckan_api)).
:- use_module(library(uri_ext)).

:- meta_predicate
    ckan_export(+, 2, +, +).





%! ckan_export(+Uri:atom) is det.
%! ckan_export(+Uri:atom, -Directory:atom) is det.

ckan_export(Uri) :-
  ckan_export(Uri, _).


ckan_export(Uri, Dir) :-
  (var(Dir) -> working_directory(Dir) ; true),
  ckan_site_read(Uri),
  ckan_export(Dir, ckan_group, Uri, group),
  ckan_export(Dir, ckan_license, Uri, license),
  ckan_export(Dir, ckan_organization, Uri, organization),
  ckan_export(Dir, ckan_package, Uri, package),
  ckan_export(Dir, ckan_resource, Uri, resource),
  ckan_export(Dir, ckan_tag, Uri, tag),
  ckan_export(Dir, ckan_user, Uri, user).

ckan_export(Dir, Mod:Goal_2, Uri, Category) :-
  flag(Goal_2, _, 0),
  file_name_extension(Category, 'json.gz', Local),
  directory_file_path(Dir, Local, File),
  setup_call_cleanup(
    gzopen(File, write, Out),
    (
      format(Out, "[~n", []),
      forall(
        catch(call(Mod:Goal_2, Uri, Dict), E, true),
        (   var(E)
        ->  json_write_dict(Out, Dict),
            format(Out, ",~n", []),
            flag(Goal_2, N, N+1),
            format(user_output, "~a: ~D~n", [Goal_2,N])
        ;   print_message(warning, E)
        )
      ),
      format(Out, "]~n", [])
    ),
    close(Out)
  ).
