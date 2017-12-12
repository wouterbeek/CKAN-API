:- module(
  ckan2json,
  [
    ckan2json/1 % +Site
  ]
).

/** <module> CKAN2JSON

@author Wouter Beek
@version 2017/12
*/

:- use_module(library(http/ckan_api)).
:- use_module(library(http/json)).
:- use_module(library(zlib)).

:- meta_predicate
    ckan2json(2, +, +).





%! ckan2json(+Site) is det.

ckan2json(Site) :-
  ckan_site_read(Site),
  %ckan2json(ckan_group, Site, group),
  %ckan2json(ckan_license, Site, license),
  %ckan2json(ckan_organization, Site, organization),
  %ckan2json(ckan_package, Site, package),
  %ckan2json(ckan_resource, Site, resource),
  ckan2json(ckan_tag, Site, tag),
  ckan2json(ckan_user, Site, user).

ckan2json(Mod:Goal_2, Site, Name) :-
  flag(Goal_2, _, 0),
  format(atom(File), '~a.json.gz', [Name]),
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
