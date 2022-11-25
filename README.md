This is a formally verified SAT-based planner for planning problems specified in a fragment of Fast-Downward's [translator format](), which is a language to represent SAS+ planning problems. Note, however, that this planner does not support conditional effects as well as metric, mutex, and axiom sections of the translator format.

This planner was described in the following paper:

  Mohammad Abdulaziz and Friedrich Kurz. Formally Verified SAT-Based Planning. In AAAI Conference on Artificial Intelligence (AAAI), 2023.


Verified SAT-Based AI Planning
==============================

 1) Download and install [Isabelle/HOL](https://isabelle.in.tum.de)

 2) Install the Archive of Formal Proofs as indicated in [this
 page](https://www.isa-afp.org/using.shtml). We require version = Isabelle-2022,
 which, at the time of writing, is the current version.

 3) Generate sml code equivalent to the verified Isabelle encoding by running

    cd afp_download_path/thys/Verified_SAT_Based_AI_Planning

    isabelle_download_directory/bin/isabelle build -ecv -d . -o document=pdf -o document_output=. Verified_SAT_Based_AI_Planning

  This will invoke Isabelle to check all proofs and re-generate the
  exported code, which is written to <code> afp_download_path/thys/AI_Planning_Languages_Semantics/code/encode_problem.sml</code> and  <code> afp_download_path/thys/AI_Planning_Languages_Semantics/code/decode_model.sml</code>.

 4) Download and install [MLton](http://mlton.org/) compiler version >= 20210117.

 5) Build the generated sml code together with the pddl parser by running the
 following commands from the top directory
  
    ./build.sh afp_download_path


 6) You can use it to solve a problem by running

    ./compute_plan.sh 100 ../example/outpus.sas
    kissat-master/build/kissat

    The first argument is the horizon, the second is the problem to
    solve which should be in Fast Downward's translator formtat, and
    the third is a path to a SAT solver executable.


Developers
==========

Mohammad Abdulaziz and Friedrich Kurz