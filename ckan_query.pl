:- module(
  ckan_query,
  [
    ckan/4,        % ?S, ?P, ?O, +File
    ckan_formats/0
  ]
).

/** <module> CKAN Query

Query locally cached CKAN metadata.

@author Wouter Beek
@version 2017/04, 2017/06
*/

:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(yall)).

:- discontiguous
    media_type/1,
    media_type/2,
    rdf_format/1,
    rdf_format/2,
    rdf_media_type/1,
    rdf_media_type/2.

:- rdf_register_prefix(ckan, 'https://lodlaundromat.org/ckan/').

:- rdf_meta
   ckan(r, r, o, ?).





%! ckan(?S, ?P, ?O, +File) is nondet.
%
% File must be an HDT file created by library(semweb/ckan2rdf).

ckan(S, P, O, File) :-
  hdt_call_on_file(File, {S,P,O}/[Hdt]>>hdt_search(Hdt, S, P, O)).



%! ckan_formats is det.

ckan_formats :-
  findall(
    Format-dummy,
    ckan(_, ckan:format, Format^^xsd:string, _),
    Pairs
  ),
  sort(1, @=<, Pairs, SortedPairs),
  group_pairs_by_key(SortedPairs, JoinedPairs),
  maplist(number_of_values0, JoinedPairs, NumberPairs),
  transpose_pairs(NumberPairs, InvPairs),
  keysort(InvPairs, Rows),
  maplist(writeln, Rows).

number_of_values0(Key-Vals, Key-NumVals) :-
  length(Vals, NumVals).



%! ckan_resource_uri(-Uri) is nondet.

ckan_resource_uri(Uri) :-
  ckan(S, ckan:format, Format0^^xsd:string, File),
  string_lower(Format0, Format),
  (rdf_format(Format) ; rdf_format(Format, _)),
  ckan(S, ckan:url, Uri^^_, File).



%! rdf_format(?String) is nondet.
%! rdf_format(?String, ?MediaType) is nondet.

rdf_format("api/sparql").
rdf_format("application/n-triples",  application/'n-triples').
rdf_format("application/rdf xml",    application/'rdf+xml').
rdf_format("application/rdf+xml",    application/'rdf+xml').
rdf_format("application/trig",       application/trig).
rdf_format("application/x-nquads",   application/'n-quads').
rdf_format("application/x-ntriples", application/'n-triples').
rdf_format("application/x-trig",     application/trig).
rdf_format("example/n3",             text/n3).
rdf_format("example/ntriples",       application/'n-triples').
rdf_format("example/rdf xml",        application/'rdf+xml').
rdf_format("example/rdf+json",       application/'ld+json').
rdf_format("example/rdf+ttl",        text/turtle).
rdf_format("example/rdf+xml",        application/'rdf+xml').
rdf_format("example/rdfa",           application/'xhtml+xml').
rdf_format("example/turtle",         text/turtle).
rdf_format("example/x-turtle",       text/turtle).
rdf_format("gzip::nquads",           application/'n-quads').
rdf_format("html+rdfa",              application/'xhtml+xml').
rdf_format("linked data").
rdf_format("mapping/owl").
rdf_format("mapping/rdfs").
rdf_format("meta/owl").
rdf_format("meta/rdf-schema").
rdf_format("meta/void").
rdf_format("n-quads",                application/'n-quads').
rdf_format("n-triples",              application/'n-triples').
rdf_format("owl").
rdf_format("owl, ontology, meta/owl").
rdf_format("rdf").
rdf_format("rdf-n3",                 text/n3).
rdf_format("rdf-turtle",             text/turtle).
rdf_format("rdf-xml",                application/'rdf-xml').
rdf_format("rdf/n3",                 text/n3).
rdf_format("rdf/turtle",             text/turtle).
rdf_format("rdf/xml, html, json").
rdf_format("rdfa",                   application/'xhtml+xml').
rdf_format("sparql").
rdf_format("sparql-query").
rdf_format("sparql web form").
rdf_format("text/n3",                text/n3).
rdf_format("text/rdf+n3",            text/n3).
rdf_format("text/rdf+ttl",           text/turtle).
rdf_format("text/turtle",            text/turtle).
rdf_format("ttl",                    text/turtle).



%! rdf_media_type(?String) is nondet.
%! rdf_media_type(?String, ?MediaType) is nondet.

rdf_media_type("api/sparql").
rdf_media_type("application/json-ld",             application/'ld+json').
rdf_media_type("application/ld+json",             application/'ld+json').
rdf_media_type("application/n-quads",             application/'n-quads').
rdf_media_type("application/n-triples",           application/'n-triples').
rdf_media_type("application/owl+xml",             application/'rdf+xml').
rdf_media_type("application/rdf xml",             application/'rdf+xml').
rdf_media_type("application/rdf+json",            application/'ld+json').
rdf_media_type("application/rdf+n3",              text/n3).
rdf_media_type("application/rdf+turtle",          text/turtle).
rdf_media_type("application/rdf+xml; qs=0.9",     application/'rdf+xml').
rdf_media_type("application/rdf\\+xml",           application/'rdf+xml').
rdf_media_type("application/sparql-results+json application/sparql-results+xml ").
rdf_media_type("application/turtle",              text/turtle).
rdf_media_type("application/rdf+n3",              text/n3).
rdf_media_type("application/rdf+xml",             application/'rdf+xml').
rdf_media_type("application/sparql-results+json", application/'sparql-results+json').
rdf_media_type("application/sparql-results+xml",  application/'sparql-results+xml').
rdf_media_type("application/x-nquads",            application/'n-quads').
rdf_media_type("application/x-ntriples",          application/'n-triples').
rdf_media_type("application/x-turtle",            text/turtle).
rdf_media_type("application/xhtml+xml",           application/'xhtml+xml').
rdf_media_type("example/rdf+xml",                 application/'rdf+xml').
rdf_media_type("rdf/xml",                         application/'rdf+xml').
rdf_media_type("xml/rdf",                         application/'rdf+xml').
rdf_media_type("text/n3",                         text/n3).
rdf_media_type("text/rdf").
rdf_media_type("text/rdf+json",                   application/'ld+json').
rdf_media_type("text/rdf+n3",                     text/n3).
rdf_media_type("text/turtle",                     text/turtle).



%! media_type(?String, ?MediaType) is nondet.

media_type("Shapefile").
media_type("application").
media_type("application/ html",            text/html).
media_type("application/application/csv",  text/csv).
media_type("application/atom+xml",         application/'atom+xml').
media_type("application/bzip2",            application/'x-bzip2').
media_type("application/csv",              text/csv).
media_type("application/download").
media_type("application/esri").
media_type("application/force-download").
media_type("application/gzip",             application/gzip).
media_type("application/json",             application/json).
media_type("application/marc",             application/marc).
media_type("application/msword",           application/msword).
media_type("application/octect-stream",    application/'octet-stream').
media_type("application/opensearchdescription+xml").
media_type("application/pdf",              application/pdf).
media_type("application/rar",              application/'x-rar-compressed').
media_type("application/rss+xml",          application/'rss+xml').
media_type("application/sql",              application/sql).
media_type("application/vnd.google-earth.kml+xml", application/'vnd.google-earth.kml+xml').
media_type("application/vnd.ms-excel",     application/'vnd.ms-excel').
media_type("application/vnd.ms-excel.12",  application/'vnd.ms-excel.sheet.binary.macroEnabled.12').
media_type("application/vnd.ms-excel.sheet.binary.macroenabled.12", application/'vnd.ms-excel.sheet.binary.macroEnabled.12').
media_type("application/vnd.oasis.opendocument.spreadsheet", application/'vnd.oasis.opendocument.spreadsheet').
media_type("application/vnd.oasis.opendocument.text", application/'vnd.oasis.opendocument.text').
media_type("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet').
media_type("application/vnd.openxmlformats-officedocument.wordprocessingml.document", application/'vnd.openxmlformats-officedocument.wordprocessingml.document').
media_type("application/x-bibtex").
media_type("application/x-bzip").
media_type("application/x-bzip2").
media_type("application/x-excel",          application/'vnd.ms-excel').
media_type("application/x-file").
media_type("application/x-gzip",           application/gzip).
media_type("application/x-javascript",     application/javascript).
media_type("application/x-netcdf").
media_type("application/x-pdf",            application/pdf).
media_type("application/x-rar-compressed", application/'vnd.rar').
media_type("application/x-tar").
media_type("application/x-unknown").
media_type("application/x-zip").
media_type("application/x-zip-compressed").
media_type("application/xhtml+xml, text/plain, application/rdf+json,application/rdf+xml.text/rdf+n3").
media_type("application/xls").
media_type("application/xml",              text/xml).
media_type("application/xml-sitemap").
media_type("application/zip").
media_type("csv",                          text/csv).
media_type("html",                         text/html).
media_type("image/gif").
media_type("image/jpeg").
media_type("image/png",                    image/png).
media_type("image/tiff",                   image/tiff).
media_type("kml",                          application/'vnd.google-earth.kml+xml').
media_type("multipart/form-data").
media_type("rd").
media_type("tdyn/html").
media_type("text/comma-separated-values",  text/csv).
media_type("text/csv",                     text/csv).
media_type("text/html",                    text/html).
media_type("text/javascript",              application/javascript).
media_type("text/plain",                   application/plain).
media_type("text/rss",                     application/'rss+xml').
media_type("text/sql",                     application/sql).
media_type("text/tab-separated-values",    text/'tab-separated-values').
media_type("text/tsv",                     text/'tab-separated-values').
media_type("text/vnd.graphviz",            text/'vnd.graphviz').
media_type("text/x-csv",                   text/csv).
media_type("text/x-sql",                   application/sql).
media_type("text/xml",                     text/xml).
media_type("te").
media_type("text/html, application/XML, text/plain, application/sparql-results+json, application/sparql-results+xml, application/rdf+xml, text/rdf+n3, application/x-turtle, application/x-trig, application/trix, text/boolean").
media_type("text/html, application/XML, text/plain, application/sparql-results+json, application/sparql-results+xml, application/rdf+xml,, text/rdf+n3, application/x-turtle, application/x-trig, application/trix, text/boolean").
media_type("wsf/gml").



%! media_type(?MediaType, ?Structured:boolean, ?Open:boolean) is nondet.

media_type(application/'atom+xml',                    true,  true ).
media_type(application/gzip,                          false, true ).
media_type(application/json,                          true,  true ).
media_type(application/'ld+json',                     true,  true ).
media_type(application/marc,                          true,  true ).
media_type(application/msword,                        false, false).
media_type(application/'n-quads',                     true,  true ).
media_type(application/'n-triples',                   true,  true ).
media_type(application/'octet-stream',                false, false).
media_type(application/pdf,                           false, false).
media_type(application/'rdf+xml',                     true,  true ).
media_type(application/'rss+xml',                     true,  true ).
media_type(application/'sparql-results+xml',          true,  true ).
media_type(application/sql,                           true,  true ).
media_type(application/'vnd.google-earth.kml+xml',    true,  true ).
media_type(application/'vnd.ms-excel',                true,  false).
media_type(application/'vnd.ms-excel.sheet.binary.macroEnabled.12', true, false).
media_type(application/'vnd.oasis.opendocument.spreadsheet', false, true).
media_type(application/'vnd.oasis.opendocument.text', false, true ).
media_type(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet', false, true).
media_type(application/'vnd.openxmlformats-officedocument.wordprocessingml.document', false, true).
media_type(application/'x-bibtex',                    true,  true ).
media_type(application/'x-bzip',                      false, true ).
media_type(application/'x-bzip2',                     false, true ).
media_type(application/'x-excel',                     true,  false).
media_type(application/'x-gzip',                      false, true ).
media_type(application/'x-javascript',                true,  true ).
media_type(application/'x-netcdf',                    true,  true ).
media_type(application/'x-rar-compressed',            false, false).
media_type(application/'x-tar',                       false, true ).
media_type(application/'x-trig',                      true,  true ).
media_type(application/'x-zip-compressed',            false, false).
media_type(application/'xhtml+xml',                   true,  true ).
media_type(application/xml,                           true,  true ).
media_type(application/zip,                           false, false).
media_type(image/gif,                                 false, true ).
media_type(image/jpeg,                                false, true ).
media_type(image/png,                                 false, true ).
media_type(image/tiff,                                false, false).
media_type(multipart/'form-data',                     false, false).
media_type(text/csv,                                  true,  true ).
media_type(text/html,                                 true,  true ).
media_type(text/n3,                                   true,  true ).
media_type(text/plain,                                false, true ).
media_type(text/'tab-separated-values',               true,  true ).
media_type(text/turtle,                               true,  true ).
media_type(text/'vnd.graphviz',                       true,  true ).
media_type(text/xml,                                  true,  true ).
