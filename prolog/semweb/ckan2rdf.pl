:- module(
  ckan2rdf,
  [
    ckan2rdf/1,        % +Dir
    ckan2rdf_thread/1, % +Dir
    ckan2rdf/1         % +Dir, +Site
  ]
).

/** <module> CKAN-2-RDF

http://datos.santander.es

@author Wouter Beek
@version 2017/04, 2017/06, 2017/09
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(hash_ext)).
:- use_module(library(hdt)).
:- use_module(library(http/ckan_api)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(semweb/turtle)).
:- use_module(library(zlib)).

:- debug(ckan2rdf).

:- nodebug(http_client).

:- rdf_register_prefix(ckan, 'https://lodlaundromat.org/ckan/').

:- rdf_meta
   key_datatype(+, r).





%! ckan2rdf(+Dir) is det.
%! ckan2rdf(+Dir, +Site) is det.

ckan2rdf(Dir) :-
  forall(
    ckan_site_uri(Site),
    ckan2rdf(Dir, Site)
  ).


ckan2rdf(Dir, Site) :-
  md5(Site, Hash),
  maplist(hash_file(Dir, Hash), ['data.nt','data.hdt'], [File1,File2]),
  ckan2rdf(Site, Hash, File1, File2).

ckan2rdf(Site, Hash, _, File2) :-
  exists_file(File2), !,
  debug(ckan2rdf, "Skipping: ~a ~a", [Hash,Site]).
ckan2rdf(Site, Hash, File1, File2) :-
  debug(ckan2rdf, "Started: ~a ~a", [Hash,Site]),
  atomic_list_concat([graph,Hash], /, Local),
  rdf_global_id(ckan:Local, G),
  rdf_retractall(_, _, _, G),
  scrape_site(G, Site),
  rdf_save_ntriples(File1, [graph(G)]),
  rdf_retractall(_, _, _, G),
  hdt_create_from_file(File2, File1, []),
  debug(ckan2rdf, "Finished: ~a ~a", [Hash,Site]).

scrape_site(G, Site) :-
  rdf_assert(Site, rdf:type, ckan:'Site', G),
  forall(
    ckan_group(Site, Dict),
    (
      assert_pair(G, Site, containsGroup, Dict),
      ckan_debug(Site, group)
    )
  ),
  forall(
    ckan_license(Site, Dict),
    (
      assert_pair(G, Site, containsLicense, Dict),
      ckan_debug(Site, license)
    )
  ),
  forall(
    ckan_organization(Site, Dict),
    (
      assert_pair(G, Site, containsOrganization, Dict),
      ckan_debug(Site, organization)
    )
  ),
  forall(
    ckan_package(Site, Dict),
    (
      assert_pair(G, Site, containsPackage, Dict),
      ckan_debug(Site, package)
    )
  ),
  forall(
    ckan_resource(Site, Dict),
    (
      assert_pair(G, Site, containsResource, Dict),
      ckan_debug(Site, resource)
    )
  ),
  forall(
    ckan_tag(Site, Dict),
    (
      assert_pair(G, Site, containsTag, Dict),
      ckan_debug(Site, tag)
    )
  ),
  forall(
    ckan_user(Site, Dict),
    (
      assert_pair(G, Site, containsUser, Dict),
      ckan_debug(Site, user)
    )
  ).

ckan_debug(Hash, Type) :-
  atomic_list_concat([Hash,Type], :, Flag),
  flag(Flag, N, N+1),
  debug(ckan2rdf2, "~a: ~D", [Flag,N]).

assert_pair(_, _, _, "") :- !.
assert_pair(_, _, _, null) :- !.
assert_pair(_, _, agent, _) :- !.
assert_pair(_, _, archiver, _) :- !.
assert_pair(_, _, concepts_eurovoc, _) :- !.
assert_pair(_, _, config, _) :- !.
assert_pair(_, _, default_extras, _) :- !.
assert_pair(_, _, extras, _) :- !.
assert_pair(_, _, keywords, _) :- !.
assert_pair(_, _, pids, _) :- !.
assert_pair(_, _, qa, _) :- !.
assert_pair(_, _, relationships_as_object, _) :- !.
assert_pair(_, _, relationships_as_subject, _) :- !.
assert_pair(_, _, status, _) :- !.
assert_pair(_, _, tracking_summary, _) :- !.
assert_pair(G, I, Key, L) :-
  is_list(L), !,
  maplist(assert_pair(G, I, Key), L).
assert_pair(G, I1, Key, Dict) :-
  is_dict(Dict), !,
  dict_pairs(Dict, _, Pairs1),
  (   selectchk(id-Lex, Pairs1, Pairs2)
  ->  pairs_keys_values(Pairs2, Keys, Vals),
      atom_string(Id, Lex),
      key_predicate_class(Key, LocalP, Local),
      rdf_global_id(ckan:LocalP, P),
      atom_capitalize(Local, CLocal),
      rdf_global_id(ckan:CLocal, C),
      atomic_list_concat([Local,Id], /, LocalI),
      rdf_global_id(ckan:LocalI, I2),
      rdf_assert(I2, rdf:type, C, G),
      rdf_assert(I1, P, I2, G),
      maplist(assert_pair(G, I2), Keys, Vals)
  ;   true
  ).
assert_pair(G, I, Key, Lex) :-
  rdf_global_id(ckan:Key, P),
  (   catch(xsd_time_string(_, D, Lex), _, fail)
  ->  true
  ;   memberchk(Lex, ["false","true"])
  ->  rdf_equal(xsd:boolean, D)
  ;   integer(Lex)
  ->  rdf_equal(xsd:integer, D)
  ;   rdf_equal(xsd:string, D)
  ),
  rdf_literal(Lit, D, Lex, _),
  rdf_assert(I, P, Lit, G).

key_predicate_class(activity, hasActivity, activity) :- !.
key_predicate_class(agent, hasAgent, agent) :- !.
key_predicate_class(containsGroup, containsGroup, group) :- !.
key_predicate_class(containsLicense, containsLicense, license) :- !.
key_predicate_class(containsOrganization, containsOrganization, organization) :- !.
key_predicate_class(containsPackage, containsPackage, package) :- !.
key_predicate_class(containsResource, containsResource, resource) :- !.
key_predicate_class(containsTag, containsTag, tag) :- !.
key_predicate_class(containsUser, containsUser, user) :- !.
key_predicate_class(datasets, hasDataset, dataset) :- !.
key_predicate_class(default_group_dicts, hasGroup, group) :- !.
key_predicate_class(groups, hasGroup, group) :- !.
key_predicate_class(individual_resources, hasIndividualResource, individualResource) :- !.
key_predicate_class(organization, hasOrganization, organization) :- !.
key_predicate_class(packages, hasPackage, package) :- !.
key_predicate_class(resources, hasResource, resource) :- !.
key_predicate_class(tags, hasTag, tag) :- !.
key_predicate_class(users, hasUser, user) :- !.
key_predicate_class(X, Y, Z) :-
  gtrace,
  key_predicate_class(X, Y, Z).



%! ckan2rdf_thread(+Dir) is det.
%
% Threaded version of ckan2rdf/1.

ckan2rdf_thread(Dir) :-
  forall(
    ckan_site_uri(Site),
    thread_create(ckan2rdf(Dir, Site), _, [alias(Site),detached(true)])
  ).
