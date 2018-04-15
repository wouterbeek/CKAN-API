:- module(
  ckan_export,
  [
    ckan_export/1 % +Site
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
:- use_module(library(http/ckan_api)).
:- use_module(library(uri_ext)).

:- meta_predicate
    ckan_export(2, +, +).





%! ckan_export(+Site) is det.

ckan_export(Site) :-
  ckan_site_read(Site),
  ckan_export(ckan_group, Site, group),
  ckan_export(ckan_license, Site, license),
  ckan_export(ckan_organization, Site, organization),
  ckan_export(ckan_package, Site, package),
  ckan_export(ckan_resource, Site, resource),
  ckan_export(ckan_tag, Site, tag),
  ckan_export(ckan_user, Site, user).

ckan_export(Mod:Goal_2, Site, Category) :-
  uri_comps(Site, uri(_,auth(_,_,Host,_),_,_,_)),
  now(dt(Y,M,D,_,_,_,_)),
  flag(Goal_2, _, 0),
  format(atom(File), '~a-~d-~d-~d-~a.json.gz', [Host,Y,M,D,Category]),
  setup_call_cleanup(
    gzopen(File, write, Out),
    (
      format(Out, "[~n", []),
      forall(
        call(Mod:Goal_2, Site, Dict),
        (
          json_write_dict(Out, Dict),
          format(Out, ",~n", []),
          flag(Goal_2, N, N+1),
          format(user_output, "~a: ~D~n", [Goal_2,N])
        )
      ),
      format(Out, "]~n", [])
    ),
    close(Out)
  ).
