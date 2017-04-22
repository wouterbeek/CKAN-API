:- module(
  https_open,
  [
    https_open/3 % +Uri, -In, +Opts
  ]
).

/** <module> HTTPS Open

Make it easy to send an HTTPS request in SWI-Prolog.

@author Wouter Beek
@version 2017/04
*/

:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(option)).
:- use_module(library(ssl)).

:- public
    ssl_verify/5.

ssl_verify(_SSL, _ProblemCertificate, _AllCertificates, _FirstCertificate, _Error).





%! https_open(+Uri, -In, +Opts) is det.
%
% Prints a warning and fails when an exception occurs, catching the
% following common SSL exception:
%
% ```prolog
% error(
%   ssl_error(
%     '14090086',
%     'SSL routines',
%     ssl3_get_server_certificate,
%     'certificate verify failed'
%   ),
%   _
% )
% ```

https_open(Uri, In, Opts1) :-
  merge_options(
    Opts1,
    [
      cert_verify_hook(cert_accept_any),
      timeout(1)
    ],
    Opts2
  ),
  catch(
    http_open(Uri, In, Opts2),
    E,
    (
      print_message(warning, E),
      fail
    )
  ).
