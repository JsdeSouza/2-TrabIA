0.7::r :- str(snow_covered).
0.3::r :- str(dry).
0.5::r :- str(wet).

0.9::v :- r.
0.1::v :- \+r.

0.33::str(snow_covered).
0.33::str(dry).
0.34::str(wet).


query(v).
evidence(str(snow_covered)).
