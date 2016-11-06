;;; extensions [ nw ] ;; using this extension would allow easy selection of canned network generation
;; I want to create my own network as a learning exercise with netlogo.
;; I'm not quite ready to use the canned, parameterizable networks just yet.

breed [spruces spruce]      ;; nodes in network that live on patches
breed [firs fir]            ;; nodes in network that live on patches
breed [mushrooms mushroom]  ;; nodes in network that live on patches

breed [spruce-roots spruce-root]      ;; for growing roots on patches
breed [fir-roots fir-root]            ;; for growing roots on patches
breed [mushroom-roots mushroom-root]  ;; for growing roots on patches

breed [spruce-particles spruce-particle]     ;; for diffusion creation of roots
breed [fir-particles fir-particle]           ;; for diffusion creation of roots
breed [mushroom-particles mushroom-particle] ;; for diffusion creation of roots

spruces-own [ age extended-root-set i-touch isolated? ]
firs-own [ age extended-root-set i-touch isolated? ]
mushrooms-own [ age extended-root-set i-touch isolated? ]

spruce-roots-own [ mushroom-set tree-set origin]
fir-roots-own [ mushroom-set tree-set origin ]
mushroom-roots-own [ mushroom-set tree-set origin ]

globals [ spruce-plist fir-plist mushroom-plist
  tree-pass? mushroom-pass? link-pass?  link-pass-limit links-finished?
  all-agents-set reduced-search-agentset root-radius
  absolute-tick-limit]

;;====================================
;; main routines setup and go
to setup
  clear-all

  set root-radius 4
  ;; after root propagation touch lists are interesting
  ;; trees get propagated first in the tree-pass - mushrooms get propagated after the trees
  ;; (trees, mushrooms, and roots touching on patches are interesting and turn yellow
  generate-spruce-plist   ;; patch list of where to plant spruce
  generate-fir-plist      ;; patch list of where to plant fir
  generate-mushroom-plist ;; patch list of where to plant mushrooms
  ;; Random selection uses 3 distinct calls - will duplicate locations across lists exist? see notes in Utilities Section at bottom
  ;; clean-plists         ;; for now this call is commented out -
  ;; Have the trees and mushrooms show up in the visible world
  plant-spruces
  plant-firs
  plant-mushrooms
  set tree-pass? true
  reset-ticks ;; now we are ready to do interesting stuff like grow roots and establish a connectivity graph (network)
end

to go
  let end-tree tree-pass-limit
  let end-mush end-tree + mushroom-pass-limit
  let end-links end-mush + 1  ;; we only need one tick to make links

  if (tree-pass? = true) [
    ifelse ticks = end-tree [
      set tree-pass? false                ;; stop tree root growth pass
      set mushroom-pass? true             ;; start mushroom pass
      clean-up-tree-pass
    ]
    [ execute-tree-pass ]       ;; tick limit not reached, keep on generating roots
  ]

  if (mushroom-pass? = true) [
    ifelse ticks = end-mush [
      set mushroom-pass? false            ;; stop mushroom root growth pass
      set link-pass? true                 ;; start link pass
      clean-up-mush-pass
    ]
    [ execute-mushroom-pass  ]  ;; tick limit not reached, keep on generating roots
  ]

  if (link-pass? = true) [
     ifelse ticks = end-links [
      set link-pass? false                ;; stop link pass
      stop
    ]
    [ execute-link-pass ]       ;; tick limit not reached, keep on generating roots
  ]

  ;;  add a perturbation pass here when ready

  tick
end
;; end of main routines  setup and go
;;====================================




;; *********** Start of pass code **********
;; ==== tree pass code (spruce and fir) ====
to execute-tree-pass
  ;; = = = = = = = = = = = = = = = = = = = = = = = = = = spruces first
   propagate-spruce-roots ;; makes spruce particles that wiggle and diffuse
   grow-spruces           ;; interacts with wiggling spruce particles - deciding when/where roots grow
   ;; = = = = = = = = = = = = = = = = = = = = = = = = = = spruce finished - firs next
   propagate-fir-roots   ;; makes fir particles that wiggle and diffuse
   grow-firs             ;; interacts with wiggling fir particles - deciding when/where roots grow
end
to clean-up-tree-pass
  dissolve-spruce-particles           ;; kill off unused particle agents
  dissolve-fir-particles
  update-spruces                      ;; now trees start aging when updated
  update-firs
end
;; ==== mushroom pass code =======
to execute-mushroom-pass
  propagate-mushroom-roots ;; makes mushroom particles that wiggle and diffuse
  grow-mushrooms           ;; interacts with wiggling mushroom particles - deciding when/where roots grow
end
to clean-up-mush-pass
  dissolve-mushroom-particles        ;; kill off unused particle agents
  update-mushrooms                   ;; mushrooms start aging when updated
  update-spruces                     ;; trees keep aging along with mushrooms
  update-firs
end

;; ==== link pass code =======
to execute-link-pass
  ;; only need the above ground agents - root generation has done most of the work already
  set all-agents-set turtles with [breed = spruces or breed = firs or breed = mushrooms]
  detect-isolation    ;; sets the isolated? property of all-agents-set
  set reduced-search-agentset all-agents-set with [isolated? = false]
  ;; reduced-agentset will be used for the collect touches procedure
  collect-touches   ;; use the i-touch list of each non-isolated agent to make links between pairs of connected things
  if (hide-isolated?)
    [ hide-isolated-agents ]
  ask reduced-search-agentset [
    create-links-with other turtle-set i-touch [
      set thickness 2
      set color red
    ]
  ]
end
;; ************ End of pass code ***********






;;(NOTE: we could reduce the code size if parameterization of procedures was used)
;; newbie fumbling
;; Start of Agent code
;; ===== spruce code =====
to generate-spruce-plist
  set spruce-plist (list) ;; start with empty list
  rand-spruce-pxy-coord   ;; generate random list of seed patches
end
to dissolve-spruce-particles
      ask spruce-particles with [color = pink]
      [set color black die]
end
to propagate-spruce-roots
  ;; create spruce particles that will aggregate into spruce roots around the spruce seed patches
  create-spruce-particles num-spruce-particles [
    set color pink
    set size 2 ;; easier to see
    setxy random-xcor random-ycor
  ] ; set them in motion - individual particles die after making a root - rest die later (dissolve)
  ask spruce-particles [
   right random tree-wiggle-angle
   left random tree-wiggle-angle
   forward 1
  ]
end
to plant-spruces
  ;; use the spruce patch list to sprout spruces on designated patches
  foreach spruce-plist [
    ask ? [ sprout-spruces 1
              [ ;; commands to initialize
                set age random 4
                setxy pxcor pycor
                set pcolor blue             ;; patch under spruce is blue
                set color blue + 4          ;; spruce on patch is lighter shade
                set shape "tree"
                set size 8
                set i-touch (list)          ;;  list of everything I touch (known after all root generation is complete)
                set isolated? false
                set extended-root-set no-turtles
              ]
          ]  ;; sprout
  ]  ;; foreach
end

to update-spruces
  ;; increase age
  ask spruces [ set age age + 1 ]
  ;; adjust color for age
  ask spruces [ set color scale-color blue age 10 0 ]
  ;; adjust size for age
   ask spruces [ set size (size + ((random 4) + 1)) ]
end
to grow-spruces
  ;; spruce particles are wiggling around in the world
  ask spruce-particles [
  let current-patch patch-at 0 0          ;; target patch under particle
  ;; near a patch that is color-coded for a spruce tree or spruce root ?
  ifelse (any? neighbors with [ pcolor = yellow ])
  [ ;;  some kind of root connection already established - don't make anything here to avoid duplicates
  ]
  [ ;; otherwise - not yellow
    if (any? neighbors with [ pcolor = blue ]) ;; ask particles if spruce patches (tree or root) nearby
       [ ;; blue neighbors, maybe coalesce new root
         ask current-patch [ coalesce-spruce-root self ]
       ;; - - - - - - - - - - - - - - - - - - - - - - -  touching anything?
       ;; ///// coalesce might have made a new spruce root on current patch /////
       ;; check all neighbors for blue root touching blue root with different root-owner trees (spruce root touching spruce root directly)
       let blue-n spruce-roots-on [neighbors] of current-patch
       let new-spr-root spruce-roots-on current-patch
       if any? new-spr-root
          [ if (any? blue-n)  ;; neighbor could be a tree - make sure you have to roots to check
               [ ask current-patch [ check-blue-touch-blue self ]
               ]
          ]
       ;; ///// coalesce might have made a new spruce root on current patch /////
       ;; check all neighbors for blue root touching green root (spruce root touching fir root)
       let green-n fir-roots-on [neighbors] of current-patch
       if any? new-spr-root
          [ if (any? green-n)  ;; neighbor could be a tree - make sure you have roots to check
               [ ask current-patch [ check-blue-touch-green self ]
               ]
          ]
       ;; ///// coalesce might have made a new spruce root on current patch /////
       ;; BUT - no mushroom roots just yet - so no need to check all neighbors for blue touch violet (done in mushroom pass)
       ;; - - - - - - - - - - - - - - - - - - - - - - -  touching anything?
       die;; spruce particle can now die, having completed its purpose
       ]  ;; close blue neighbors, maybe coalesce new root
  ] ;; close not yellow
 ] ;; close ask spruce particles
end
to coalesce-spruce-root [ current-patch ]
  ;; Coalesce a new spruce root on current patch
  ;; \\\\\ is a spruce tree nearby? - prefer hooking to tree first over an existing root (if-else)
  let blue-nabor-trees spruces-on [neighbors] of current-patch
  ifelse (any? blue-nabor-trees)       ;; neighbor could be a root and neighbor tree could be empty - make sure you  have tree to hook to
    [ ask one-of blue-nabor-trees
      [ ;; current patch will host a spruce root for this tree with data from neighbor
        let blue-nabor-owner self                         ;; self is one spruce tree
        ask current-patch                                 ;; self is now patch
          [ sprout-spruce-roots 1 [                     ;; commands to initialize
            setxy pxcor pycor
            set pcolor blue                             ;; patch is blue
            set color blue - 4                          ;; root is darker than patch
            set origin blue-nabor-owner
            set tree-set turtle-set blue-nabor-owner    ;; trees that spruce root knows about
            set mushroom-set no-turtles                 ;; encounter mushrooms later
          ]
          ] ;; sprout
            ;; tell neighbor tree about this new root
            ;; grab existing extended-root-set from neighbor
        let new-spr-root spruce-roots-on current-patch
        let ers [extended-root-set] of blue-nabor-owner
        ask blue-nabor-owner [ set extended-root-set (turtle-set new-spr-root ers)] ;; new root goes in front of set
      ] ;; ask neighbor tree
    ]  ;; true block ifelse
    [  ;; false block ifelse - no spruce tree as neighbor - must be one or more spruce root neighbors

      let blue-nabor-roots spruce-roots-on [neighbors] of current-patch
      if ( not any? spruce-roots-on current-patch )
        [  ;; OK to make a new one
           ;; \\\\\ just check one neighbor;s info to make new root - but need to check all neighbors for touching situations after new root is made
          if (any? blue-nabor-roots)  ;; neighbor could be a tree and neighbor could be roots empty - make sure you  have roots to check
            [ ask one-of blue-nabor-roots
              [ ;; current patch will host a spruce root for same tree as neighbor root
                let blue-nabor-owner [origin] of self             ;; self is one spruce root - origin is a tree
                ask current-patch                                 ;; self is now patch
                  [ sprout-spruce-roots 1 [ ;; commands to initialize
                    setxy pxcor pycor
                    set pcolor blue                             ;; patch is blue
                    set color blue - 4                          ;; root is darker than patch
                    set origin blue-nabor-owner
                    set tree-set turtle-set blue-nabor-owner    ;; trees that spruce root knows about
                    set mushroom-set no-turtles                 ;; encounter mushrooms later
                  ]
                  ] ;; sprout
                    ;; neighbor root's tree's extended-root-set gets this spruce root
                let new-spr-root spruce-roots-on current-patch
                let ers [extended-root-set] of blue-nabor-owner ; grab existing data first
                ask blue-nabor-owner [ set extended-root-set (turtle-set new-spr-root ers) ] ;; add new root to front of existing data
              ]
            ]
        ] ;; close spruce-roots-on current-patch check
    ] ;; close false block of ifelse
end
to check-blue-touch-blue [current-patch]
  let blue-n spruce-roots-on [neighbors] of current-patch  ;; neighbors to check could host more than one blue root
  let new-spr-root one-of spruce-roots-on current-patch    ;; for current patch should be only one
  let spruce-owner [origin] of new-spr-root
  ask blue-n                                               ;; blue-n is a set of patches
     [ let blue-n-root one-of spruce-roots-on blue-n
           ask blue-n-root
             [ let blue-n-owner origin                     ;; ask about neighbor's owner [origin]
               if (blue-n-owner != spruce-owner)
               [ ;; hook new tree-set to closest neighbor tree-set and vice versa
                 ;; figure out closest neighbor root and harvest its data
                 let blue-n-candidate blue-n with [origin = blue-n-owner] ;; candidates for closest root must be owned by neighbor
                 let closest-spr-root-patch min-one-of blue-n-candidate [distance current-patch]
                 let closest-spr-root spruce-roots-on closest-spr-root-patch
                 ;; ask closest spruce root for its tree set and tree [origin]
                 let closest-spr-tree-set [tree-set] of closest-spr-root
                 let closest-spr-tree [origin] of closest-spr-root

                 ;; grab existing tree-set data for both new and closest BEFORE any updates
                 let new-ets [tree-set] of new-spr-root             ;; toucher
                 let closest-ets [tree-set] of closest-spr-root     ;; touchee

                 ask new-spr-root [ set tree-set (turtle-set new-ets closest-spr-tree) ]      ;; toucher root knows touchee tree (last on end)
                 ask closest-spr-root [ set tree-set (turtle-set closest-ets spruce-owner) ]  ;; touchee root knows toucher tree (last on end)

                 ask new-spr-root [ set pcolor yellow ]      ;; new spruce root turns yellow
                 ask closest-spr-root [ set pcolor yellow ]  ;; closest spruce root turns yellow
                ] ;; close hook
             ] ;; close ask blue-n-root
   ] ;; close ask blue-n
end
to check-blue-touch-green [ current-patch ]
  let green-n fir-roots-on [neighbors] of current-patch     ;; neighbors to check could host more than one green root
  ;; spruce root on current patch is asked to do this check - identify which spruce root is checker
  let new-spruce-root one-of spruce-roots-on current-patch  ;; for current patch there should be only one
  ask new-spruce-root                                       ;; self is now the spruce root doing the checking
  [
    let spruce-owner origin  ;; spruce tree that owns the root doing the checking
    ask green-n              ;; green-n is a set of patches
      [ let green-n-root one-of fir-roots-on green-n
        ask green-n-root
          [ let green-n-owner origin   ;; fir tree that owns the root being checked
            ;; don't need to ask about neighbor's owner - green and blue will be different owners
            ;; just do the hook trees instructions
            let green-n-candidate green-n with [origin = green-n-owner] ;; candidates for closest root must be owned by neighbor
            let closest-fir-root-patch min-one-of green-n-candidate [distance current-patch]
            let closest-fir-root fir-roots-on closest-fir-root-patch
            ;; ask closest fir root for its tree set
            let closest-fir-tree-set [tree-set] of closest-fir-root
            let closest-fir-tree [origin] of closest-fir-root

            ;; grab existing tree-set data for both new and closest BEFORE any updates
            let new-ets [tree-set] of new-spruce-root             ;; toucher
            let closest-ets [tree-set] of closest-fir-root        ;; touchee

            ask new-spruce-root [ set tree-set (turtle-set new-ets closest-fir-tree) ]  ;; append fir tree to end of spruce's tree set
            ask closest-fir-root [ set tree-set (turtle-set closest-ets spruce-owner) ] ;; append spruce tree to end of fir's tree set

            ;; need to update mushroom-sets too - but mushroom roots have not been grown yet

            ask new-spruce-root [ set pcolor yellow ]      ;; new spruce root (blue) turns yellow
            ask closest-fir-root [ set pcolor yellow ]     ;; closest fir root (green) turns yellow
          ] ;; close ask green root
      ] ;; close ask green neighbors
  ] ; close ask spruce root
end

;; ===== fir code =====
to generate-fir-plist
  set fir-plist (list)  ;; start with empty list
  rand-fir-pxy-coord    ;; generate random list of seed patches
end
to dissolve-fir-particles
   ask fir-particles with [color = red]
   [set color black die]
end
to propagate-fir-roots
  ;; create random fir particles that will aggregate into fir roots around the fir seed patches
  create-fir-particles num-fir-particles
  [
    set color red
    set size 2 ;; easier to see
    setxy random-xcor random-ycor
  ]
  ask fir-particles
  [ ;; set them in motion - individual particles die after making a root - rest die later (dissolve)
    right random tree-wiggle-angle
    left random tree-wiggle-angle
    forward 1
  ]
end
to plant-firs
  ;; use the fir patch list to sprout firs on designated patches
  foreach fir-plist [
    ask ? [ sprout-firs 1 [
               set age random 3
               setxy pxcor pycor
               set pcolor green                  ;; patch under fir is green
               set color green + 4               ;; fir on patch is lighter shade
               set shape "tree"
               set size 8
               set i-touch (list)                ;;  list of everything I touch (after all root generation is complete)
               set isolated? false
               set extended-root-set no-turtles
               ]
    ] ;; ask
  ] ;; foreach
end

to grow-firs
  ;; fir particles are wiggling around in the world
  ask fir-particles [
  let current-patch patch-at 0 0          ;; target patch under particle
  ;; near a patch that is color-coded for a fir tree or fir root ?
  ifelse (any? neighbors with [ pcolor = yellow ])
  [ ;;  some kind of root connection already established - don't make anything here to avoid duplicates
  ]
  [ ;; not yellow
    if (any? neighbors with [ pcolor = green ]) ;; ask particles if fir patches (tree or root) are nearby
       [ ;; green neighbors, maybe coalesce new root
         ask current-patch [ coalesce-fir-root self ]
       ;; - - - - - - - - - - - - - - - - - - - - - - -  touching anything?
       ;; ///// coalesce might have made a new fir root on current patch /////
       ;; check all neighbors for green root touching green root with different root-owner trees (fir root touching fir root directly)
       let green-n fir-roots-on [neighbors] of current-patch
       let new-fir-root fir-roots-on current-patch
       if any? new-fir-root
          [ if (any? green-n)  ;; neighbor could be a tree - make sure you have roots to check
               [ ask current-patch [ check-green-touch-green self ]
               ]
          ]
       ;; ///// coalesce might have made a new fir root on current patch /////
       ;; check all neighbors for green root touching blue root (fir root touching spruce root)
       let blue-n spruce-roots-on [neighbors] of current-patch
       if any? new-fir-root
          [ if (any? blue-n)  ;; neighbor could be a tree - make sure you hook to roots
               [ ask current-patch [ check-green-touch-blue self ]
               ]
          ]
       ;; ///// coalesce might have made a new fir root on current patch /////
       ;; BUT - no mushroom roots just yet - so cannot check all neighbors for blue touch violet (done in mushroom pass)
       ;; - - - - - - - - - - - - - - - - - - - - - - -  touching anything?
       die;; fir particle can now die, having completed its purpose
       ]  ;; close green neighbors, maybe coalesce new root
  ] ;; close not yellow
 ] ;; close ask fir particles
end
to coalesce-fir-root [ current-patch ]
  ;; Coalesce a new fir root on current patch
  ;; \\\\\ is a fir tree nearby? - prefer hooking to it first over an existing root (if-else)
  let green-nabor-trees firs-on [neighbors] of current-patch
  ifelse (any? green-nabor-trees)     ;; neighbor could be a root and neighbor tree could be empty - make sure you  have tree to check
    [ ask one-of green-nabor-trees
      [ ;; current patch will host a spruce root for this tree with data from neighbor
        let green-nabor-owner self             ;; self is one fir tree
        ask current-patch                      ;; self is now patch
          [ sprout-fir-roots 1 [ ;; commands to initialize
            setxy pxcor pycor
            set pcolor green                             ;; patch is green
            set color green - 4                          ;; root is darker than patch
            set origin green-nabor-owner
            set tree-set turtle-set green-nabor-owner        ;; trees that fir root knows about
            set mushroom-set no-turtles                      ;; encounter mushrooms later
          ]
          ] ;; sprout
            ;; work on neighbor tree - not neighbor root - tell neighbor tree about this new root
            ;; grab existing extended-root-set from neighbor
        let new-fir-root fir-roots-on current-patch
        let ers [extended-root-set] of green-nabor-owner
        ask green-nabor-owner [ set extended-root-set (turtle-set new-fir-root ers)] ;; new root goes in front of set
      ] ;; close ask neighbor tree
    ]  ;; close true block ifelse
    [  ;; false block ifelse - no fir tree as neighbor - must be one or more fir root neighbors
      let green-nabor-roots fir-roots-on [neighbors] of current-patch
      ;; bfore making a new root on this patch, check to see if there is already one there
      if ( not any? fir-roots-on current-patch )
        [ ;; OK to make a new one
          ;; \\\\\ just check one neighbor to make new root - but need to check all neighbors for touching situations after new root is made
          if (any? green-nabor-roots)  ;; neighbor could be a tree and neighbor roots could be empty - make sure you  have roots to check
            [ ask one-of green-nabor-roots
              [ ;; current patch will host a fir root for same tree as neighbor root
                let green-nabor-owner [origin] of self            ;; self is one fir root - origin is a tree
                ask current-patch                                 ;; self is now patch
                  [ sprout-fir-roots 1 [ ;; commands to initialize
                    setxy pxcor pycor
                    set pcolor green                            ;; patch is green
                    set color green - 4                         ;; root is darker than patch
                    set origin green-nabor-owner
                    set tree-set turtle-set green-nabor-owner       ;; trees that fir root knows about
                    set mushroom-set no-turtles                     ;; encounter mushrooms later
                  ]
                  ] ;; sprout
                    ;; neighbor root's tree's extended-root-set gets this fir root
                let new-fir-root fir-roots-on current-patch
                let ers [extended-root-set] of green-nabor-owner ; grab existing data
                ask green-nabor-owner [ set extended-root-set (turtle-set new-fir-root ers) ] ;; add new root to front of existing set
              ]
            ]
        ] ;; close fir-roots-on current-patch check
    ] ;; close false block of ifelse
end
to check-green-touch-green [ current-patch ]
  let green-n fir-roots-on [neighbors] of current-patch  ;; neighbors to check could host more than one green root
  let new-fir-root one-of fir-roots-on current-patch    ;; for current patch should be only one
  let fir-owner [origin] of new-fir-root
  ask green-n   ;; green-n is a set of patches
     [ let green-n-root one-of fir-roots-on green-n
           ask green-n-root
             [ let green-n-owner origin
               ;; ask about neighbor's owner
               if (green-n-owner != fir-owner)
               [ ;; hook new tree-set to closest neighbor tree-set vice versa
                 ;; figure out closest neighbor root and harvest its data
                 let green-n-candidate green-n with [origin = green-n-owner] ;; candidates for closest root must be owned by neighbor
                 let closest-fir-root-patch min-one-of green-n-candidate [distance current-patch]
                 let closest-fir-root fir-roots-on closest-fir-root-patch
                 ;; ask closest fir root for its tree set and tree [origin]
                 let closest-fir-tree-set [tree-set] of closest-fir-root
                 let closest-fir-tree [origin] of closest-fir-root

                 ;; grab existing tree-set data for both new and closest BEFORE any updates
                 let new-ets [tree-set] of new-fir-root             ;; toucher
                 let closest-ets [tree-set] of closest-fir-root     ;; touchee

                 ask new-fir-root [ set tree-set (turtle-set new-ets closest-fir-tree) ]      ;; toucher root knows touchee tree (last on end)
                 ask closest-fir-root [ set tree-set (turtle-set closest-ets fir-owner) ]     ;; touchee root knows toucher tree (last on end)

                 ask new-fir-root [ set pcolor yellow ]      ;; new fir root turns yellow
                 ask closest-fir-root [ set pcolor yellow ]  ;; closest fir root turns yellow
                ] ;; close hook
             ] ;; close ask green-n-root
   ] ;; close ask green-n
end
to check-green-touch-blue [ current-patch ]
  let blue-n spruce-roots-on [neighbors] of current-patch     ;; neighbors to check could host more than one blue root
  ;; fir root on current patch is asked to do this check - identify which fir root is the checker
  let new-fir-root one-of fir-roots-on current-patch          ;; for current patch there should be only one

  ask new-fir-root        ;; self is now the fir root doing the checking
  [
    let fir-owner origin  ;; fir tree that owns the root doing the checking
    ask blue-n              ;; blue -n is a set of patches
      [ let blue-n-root one-of spruce-roots-on blue-n
        ask blue-n-root
          [ let blue-n-owner origin   ;; spruce tree that owns the root being checked
            ;; don't need to ask about neighbor's owner - green and blue will be different owners
            ;; just do the hook trees instructions
            let blue-n-candidate blue-n with [origin = blue-n-owner] ;; candidates for closest root must be owned by neighbor
            let closest-spr-root-patch min-one-of blue-n-candidate [distance current-patch]
            let closest-spr-root spruce-roots-on closest-spr-root-patch
            ;; ask closest spruce root for its tree set and tree [origin]
            let closest-spr-tree-set [tree-set] of closest-spr-root
            let closest-spr-tree [origin] of closest-spr-root

            ;; grab existing tree-set data for both new and closest BEFORE any updates
            let new-ets [tree-set] of new-fir-root                ;; toucher
            let closest-ets [tree-set] of closest-spr-root        ;; touchee

            ask new-fir-root [ set tree-set (turtle-set new-ets closest-spr-tree) ]  ;; append spruce tree to end of fir's tree set
            ask closest-spr-root [ set tree-set (turtle-set closest-ets fir-owner) ] ;; append fir tree to end of spruce's tree set

            ;; need to update mushroom-sets too - but mushroom roots have not been grown yet

            ask new-fir-root [ set pcolor yellow ]         ;; new fir root (green) turns yellow
            ask closest-spr-root [ set pcolor yellow ]     ;; closest spruce root (blue) turns yellow
          ] ;; close ask blue root
      ] ;; close ask blue neighbors
  ] ;; close ask fir root
end

to update-firs
  ;; increase age
  ask firs [ set age age + 1 ]
  ;; adjust color for age
  ask firs [ set color scale-color green age 10 0 ]
  ;; adjust size for age
  ask firs [ set size (size + (age mod ((random 3) + 1))) ]
end

;; ===== mushroom code =====
to generate-mushroom-plist
  set mushroom-plist (list)  ;; start with empty list
  rand-mushroom-pxy-coord    ;; generate random list of seed patches
end
to dissolve-mushroom-particles
      ask mushroom-particles with [color = orange]
      [set color black die]
end
to propagate-mushroom-roots
  ;; create random mushroom particles that look for a mushroom/mushroom root to coalesce around
  create-mushroom-particles num-mushroom-particles [
    set color orange
    set size 2 ;; easier to see
    setxy random-xcor random-ycor
  ]
  ask mushroom-particles
  [ ;; set them in motion - individual particles die after making a root - rest die later (dissolve)
    right random mushroom-wiggle-angle
    left random mushroom-wiggle-angle
    forward 1
  ]
end
to plant-mushrooms
  ;; use the mushroom patch list to sprout mushrooms on designated patches
  foreach mushroom-plist [
    ask ? [ sprout-mushrooms 1 [     ;; commands to initialize
               set age random 3
               setxy pxcor pycor
               set pcolor violet     ;; patch under mushroom is violet
               set color violet + 4  ;; mushroom on patch is lighter shade
               set shape "mushroom"
               set size 6
               set i-touch (list)    ;;  list of everything I touch (after all root generation is complete)
               set isolated? false
               set extended-root-set no-turtles
               ]
         ] ;; sprout
    ] ;; close foreach
end

to update-mushrooms
  ;; increase age
  ask mushrooms [ set age age + 1 ]
  ;; adjust color for age
  ask mushrooms [ set color scale-color violet age 10 0 ]
  ;; adjust size for age
  ask mushrooms [ set size (size + (age mod ((random 3) + 1))) ]
end
to grow-mushrooms
  ;; mushroom particles are wiggling around in the world
  ask mushroom-particles [
  let current-patch patch-at 0 0          ;; target patch under particle
  ;; near a patch that is color-coded for a muahroom or mushroom root ?
  ifelse (any? neighbors with [ pcolor = yellow ])
  [ ;;  some kind of root connection already established - don't make anything here to avoid duplicates
  ]
  [ ;; otherwise - not yellow
    if (any? neighbors with [ pcolor = violet ]) ;; ask particles if mushroom patches (mush or mush root) nearby
       [ ;; violet neighbors, maybe coalesce new root
         ask current-patch [ coalesce-mushroom-root self ]
       ;; - - - - - - - - - - - - - - - - - - - - - - -  touching anything?
       ;; ///// coalesce might have made a new muah root on current patch /////
       ;; check all neighbors for violet root touching violet root with different root-owner mushrooms (mushroom root touching mushroom root directly)
       let violet-n mushroom-roots-on [neighbors] of current-patch
       let new-mush-root mushroom-roots-on current-patch
       if any? new-mush-root
          [ if (any? violet-n)  ;; neighbor could be a mushroom - make sure you have roots to check
               [ ask current-patch [ check-violet-touch-violet self ]
               ]
          ]
       ;; ///// coalesce might have made a new mush root on current patch /////
       ;; check all neighbors for violet root touching blue root (mush root touching spruce root)
       let blue-n spruce-roots-on [neighbors] of current-patch
       if any? new-mush-root
          [ if (any? blue-n)  ;; neighbor could be a tree - make sure you hook to roots
               [ ask current-patch [ check-violet-touch-blue self ]
               ]
          ]
       ;; ///// coalesce might have made a new mush root on current patch /////
       ;; check all neighbors for violet root touching green root (mush root touching fir root)
       let green-n fir-roots-on [neighbors] of current-patch
       if any? new-mush-root
          [ if (any? green-n)  ;; neighbor could be a tree - make sure you have roots to check
               [ ask current-patch [ check-violet-touch-green self ]
               ]
          ]
       ;; - - - - - - - - - - - - - - - - - - - - - - -  touching anything?
       die;; mushroom particle can now die, having completed its purpose
       ]  ;; close violet neighbors, maybe coalesce new root
  ] ;; close not yellow
 ] ;; close ask mushroom particles
end
to coalesce-mushroom-root [ current-patch ]
  ;; Coalesce a new mush root on current patch
  ;; \\\\\ is a mushroom nearby? - prefer hooking to it first over an existing root (if-else)
  let violet-nabor-mushrooms mushrooms-on [neighbors] of current-patch
  ifelse (any? violet-nabor-mushrooms)     ;; neighbor could be a root and neighbor mushroom could be empty - make sure you  have mushroom to check
    [ ask one-of violet-nabor-mushrooms
      [ ;; current patch will host a mush root for this tree
        let violet-nabor-owner self             ;; self is one mushroom
        ask current-patch                       ;; self is now patch
          [ sprout-mushroom-roots 1 [                       ;; commands to initialize
            setxy pxcor pycor
            set pcolor violet                               ;; patch is violet
            set color violet - 4                            ;; root is darker than patch
            set origin violet-nabor-owner                   ;; owner of new roots
            set tree-set no-turtles                         ;; encounter trees elsewhere
            set mushroom-set violet-nabor-owner
            ]
          ] ;; sprout
        ;; work on neighbor mushroom - not neighbor mushroom root tell neighbor mushroom about this new root
        ;; grab existing extended-root-set from neighbor
        let new-mush-root mushroom-roots-on current-patch
        let ers [extended-root-set] of violet-nabor-owner
        ask violet-nabor-owner [ set extended-root-set (turtle-set new-mush-root ers)] ;; new root goes in front of set
      ] ;; close ask neighbor tree
    ] ;; close true block ifelse
    [ ;; false block ifelse - no mushroom as neighbor - must be one or more mush root neighbors
      let violet-nabor-roots mushroom-roots-on [neighbors] of current-patch
      if ( not any? mushroom-roots-on current-patch )
      [ ;; OK to make a new root here
      ;; \\\\\ just check one neighbor to make new root - but need to check all neighbors for touching situations after new root is made
      if (any? violet-nabor-roots)  ;; neighbor could be a mushroom and neighbor mushroom roots could be empty - make sure you have roots to check
        [ ask one-of violet-nabor-roots
          [ ;; current patch will host a  mushroom root for same mushroom as neighbor root
            let violet-nabor-owner [origin] of self           ;; self is one mushroom root - origin is a mushroom
            ask current-patch                                 ;; self is now patch
              [ sprout-mushroom-roots 1 [                     ;; commands to initialize
                setxy pxcor pycor
                set pcolor violet                             ;; patch is violet
                set color violet - 4                          ;; root is darker than patch
                set origin violet-nabor-owner
                set tree-set no-turtles                       ;; encounter trees elsewhere
                set mushroom-set violet-nabor-owner
                ]
              ] ;; sprout
            ;; neighbor root's mushroom's extended root set gets this mush root
            let new-mush-root mushroom-roots-on current-patch
            let ers [extended-root-set] of violet-nabor-owner ; grab existing data
            ask violet-nabor-owner [ set extended-root-set (turtle-set new-mush-root ers) ] ;; add new root to front of existing set
          ] ;; close ask violet root
        ]  ;; close ask violet neighbor
      ] ;; close mushroom-roots-on current-patch check
    ] ;; close false block ifelse
end
to check-violet-touch-violet [ current-patch ]
  let violet-n mushroom-roots-on [neighbors] of current-patch  ;; neighbors to check could host more than one violet root
  let new-mush-root one-of mushroom-roots-on current-patch     ;; for current patch should be only one
  let mush-owner [origin] of new-mush-root
  ask violet-n   ;; violet-n is a set of patches
     [ let violet-n-root one-of mushroom-roots-on violet-n
           ask violet-n-root
             [ let violet-n-owner origin                   ;; ask about neighbor's owner
               if (violet-n-owner != mush-owner)
               [ ;; hook new mushroom set to closest neighbor mushroom set and vice versa
                 ;; figure out closest neighbor root and harvest its data
                 let violet-n-candidate violet-n with [origin = violet-n-owner] ;; candidates for closest root must be owned by neighbor
                 let closest-mush-root-patch min-one-of violet-n-candidate [distance current-patch]
                 let closest-mush-root mushroom-roots-on closest-mush-root-patch

                 ;; ask closest mush root for its mushroom set and mushroom
                 let closest-mush-set [mushroom-set] of closest-mush-root
                 let closest-mroom [origin] of closest-mush-root
                 let pair (turtle-set mush-owner closest-mroom)                                ;; guarantee mushroom and mushroom pair up

                 ;; grab existing mushroom-set data for both new and closest BEFORE any updates
                 let new-ems [mushroom-set] of new-mush-root             ;; toucher
                 let closest-ems [mushroom-set] of closest-mush-root     ;; touchee

                 ;; update mushroom set data
                 ask new-mush-root [ set mushroom-set (turtle-set new-ems pair) ]                    ;; toucher root knows touchee root (last on end)
                 ask closest-mush-root [ set mushroom-set (turtle-set closest-ems pair) ]            ;; touchee root knows toucher root (last on end)

                 ask new-mush-root [ set pcolor yellow ]      ;; new mush root (violet) turns yellow
                 ask closest-mush-root [ set pcolor yellow ]  ;; closest mush root (violet) turns yellow
                ] ;; close hook
             ] ;; close ask violet root
     ] ;; close ask violet neighbors
end
to check-violet-touch-blue [ current-patch ]
  let blue-n spruce-roots-on [neighbors] of current-patch           ;; neighbors to check could host more than one blue root
  ;; mush root on current patch is asked to do this check - identify which mush root is the checker
  let new-mush-root one-of mushroom-roots-on current-patch          ;; for current patch there should be only one
  ask new-mush-root        ;; self is now the mush root doing the checking
  [
    let mush-owner origin   ;; mushroom that owns the root doing the checking
    ask blue-n              ;; blue-n is a set of patches
      [ let blue-n-root one-of spruce-roots-on blue-n
        ask blue-n-root
          [ let blue-n-owner origin   ;; spruce tree that owns the root being checked
            ;; don't need to ask about neighbor's owner - violet and blue will be different owners
            ;; just do the hook mushroom set instructions
            let blue-n-candidate blue-n with [origin = blue-n-owner] ;; candidates for closest root must be owned by neighbor
            let closest-spr-root-patch min-one-of blue-n-candidate [distance current-patch]
            let closest-spr-root spruce-roots-on closest-spr-root-patch
            ;; ask closest mush root for its mushroom set and mushroom [origin]
            let closest-mush-set [mushroom-set] of closest-spr-root
            let closest-mroom [origin] of closest-spr-root

            ;; grab existing mushroom-set data for both new and closest BEFORE any updates
            let new-ems [mushroom-set] of new-mush-root                ;; toucher
            let closest-ems [mushroom-set] of closest-spr-root         ;; touchee

            ask new-mush-root [ set mushroom-set (turtle-set new-ems mush-owner) ]        ;; append mushroom to end of mushroom root's set
            ask closest-spr-root [ set mushroom-set (turtle-set closest-ems mush-owner) ] ;; append mushroom to end of spruce root's set

            ;; need to update tree sets too
            ;; blue root needs to know mushroom origin
            ;; and mushroom root needs to know about blue tree origin
            ;; grab existing tree-set data for both new and closest BEFORE any updates
            let new-ets [tree-set] of new-mush-root
            let closest-ets [tree-set] of closest-spr-root
            let pair (turtle-set mush-owner blue-n-owner)                                    ;; guarantee mushroom and tree pair up
            ask new-mush-root [ set tree-set (turtle-set new-ets pair) ]
            ask closest-spr-root [ set tree-set (turtle-set closest-ets pair) ]

            ;; need to update both extended-root-sets (mushroom and spruce tree)
            ;; grab existing extended-root-set data for both new and closest BEFORE any updates
            ;; owners do updates of root sets
            let new-ers [extended-root-set] of mush-owner
            let closest-ers [extended-root-set] of blue-n-owner
            ask mush-owner [ set extended-root-set (turtle-set new-ers blue-n-root )]          ;; append spruce root to mushroom's root set
            ask blue-n-owner [ set extended-root-set (turtle-set closest-ers new-mush-root) ]  ;; append new mush root to end of spruce root set

            ask new-mush-root [ set pcolor yellow ]        ;; new mush root (violet) turns yellow
            ask closest-spr-root [ set pcolor yellow ]     ;; closest spruce root (blue) turns yellow
          ] ;; close ask blue root
      ] ;; close ask blue neighbors
  ] ;; close ask mush root
end
to check-violet-touch-green [ current-patch ]
  let green-n fir-roots-on [neighbors] of current-patch     ;; neighbors to check could host more than one green root
  ;; mushroom root on current patch is asked to do this check - identify which mushroom root is checker
  let new-mush-root one-of mushroom-roots-on current-patch  ;; for current patch there should be only one
  ask new-mush-root        ;; self is now the mush root doing the checking
  [
    let mush-owner origin    ;; mushroom that owns the root doing the checking
    ask green-n              ;; green-n is a set of patches
      [ let green-n-root one-of fir-roots-on green-n
        ask green-n-root
          [ let green-n-owner origin   ;; fir tree that owns the root being checked
            ;; don't need to ask about neighbor's owner - green and violet will be different owners
            ;; just do the hook mushroom set instructions
            let green-n-candidate green-n with [origin = green-n-owner] ;; candidates for closest root must be owned by neighbor
            let closest-fir-root-patch min-one-of green-n [distance current-patch]
            let closest-fir-root fir-roots-on closest-fir-root-patch
            ;; ask closest fir root for its mushroom set and mushroom [origin]
            let closest-mush-set [mushroom-set] of closest-fir-root
            let closest-mroom [origin] of closest-fir-root

            ;; grab existing mushroom-set data for both new and closest BEFORE any updates
            let new-ems [mushroom-set] of new-mush-root             ;; toucher
            let closest-ems [mushroom-set] of closest-fir-root        ;; touchee

            ask new-mush-root [ set mushroom-set (turtle-set new-ems mush-owner) ]           ;; append mushroom to end of mushroom's mushroom set
            ask closest-fir-root [ set mushroom-set (turtle-set closest-ems mush-owner) ]    ;; append mushroom to end of fir's mushroom set

            ;; need to update tree sets too
            ;; green root needs to know mushroom origin
            ;; and mushroom root needs to know about tree origin
            ;; grab existing tree-set data for both new and closest BEFORE any updates
            let new-ets [tree-set] of new-mush-root
            let closest-ets [tree-set] of closest-fir-root
            let pair (turtle-set mush-owner green-n-owner)                                    ;; guarantee mushroom and tree pair up
            ask new-mush-root [ set tree-set (turtle-set new-ets pair) ]
            ask closest-fir-root [ set tree-set (turtle-set closest-ets pair) ]

            ;; need to update both extended-root-sets (mushroom and fir tree)
            ;; grab existing extended-root-set data for both new and closest BEFORE any updates
            ;; owners do updates of root sets
            let new-ers [extended-root-set] of mush-owner
            let closest-ers [extended-root-set] of green-n-owner
            ask mush-owner [ set extended-root-set (turtle-set new-ers green-n-root )]          ;; append fir root to mushroom's root set
            ask green-n-owner [ set extended-root-set (turtle-set closest-ers new-mush-root) ]  ;; append new mush root to end of fir root set

            ask new-mush-root [ set pcolor yellow ]        ;; new mush root (violet) turns yellow
            ask closest-fir-root [ set pcolor yellow ]     ;; closest fir root (green) turns yellow
          ] ;; close ask green root
      ] ;; close ask green neighbors
  ] ;; close ask spruce root
end

;;====== support procedures for link pass ======
to collect-touches
  let touches no-turtles
  ;; all agents have a a tree-set and a mushroom set
  ;; only use the agents on reduced-search-agentset which has eliminted the isolated agents
  ;; - - - - - spruce collect - - - - -
  let spr-agents reduced-search-agentset with [breed = spruces]
  ask spr-agents
  [ let cur-spr self
    set touches (list)
    ask extended-root-set with [pcolor = yellow]
      [ask tree-set
      [if (not (member? self touches))
          [ set touches fput self touches ]
      ]
    ]
    ask extended-root-set with [pcolor = yellow]
      [ask mushroom-set
      [ if (not (member? self touches))
        [ set touches fput self touches ]
      ]
    ]
    set i-touch touches
  ] ;; ask spruce agents
  ;; - - - - - fir collect - - - - -
  let fir-agents reduced-search-agentset with [breed = firs]
  ask fir-agents
  [ let cur-fir self
    set touches (list)
    ask extended-root-set with [pcolor = yellow]
      [ask tree-set
      [if (not (member? self touches))
          [ set touches fput self touches ]
      ]
    ]
    ask extended-root-set with [pcolor = yellow]
      [ask mushroom-set
      [ if (not (member? self touches))
        [ set touches fput self touches ]
      ]
    ]
    set i-touch touches
  ] ;; ask fir agents
  ;; - - - - - mushroom collect - - - - -
  let mush-agents reduced-search-agentset with [breed = mushrooms]
  ask mush-agents
  [ let cur-mush self
    set touches (list)
    ask extended-root-set with [pcolor = yellow]
      [ ask tree-set
      [if (not (member? self touches))
        [ set touches fput self touches ]
      ]
    ]
    ask extended-root-set with [pcolor = yellow]
      [ask mushroom-set
      [ if (not (member? self touches))
        [ set touches fput self touches ]
      ]
    ]
    set i-touch touches
  ] ;; ask mushrooms
end

to detect-isolation
  ;; set isolation? flag in each tree and mushroom - roots do not have an isolated flag
  ; = = = = = = = = = =
    ask spruces
    [ let yellow-ers extended-root-set with [pcolor = yellow]
      ;; if spruce root extended-root-set touches anything, it will have [pcolor = yellow]
      if (not (any? yellow-ers))
      [ set isolated? true ]
    ]
  ; = = = = = = = = = =
    ask firs
    [ let yellow-ers extended-root-set with [pcolor = yellow]
      ;; if fir root extended-root-set touches anything, it will have [pcolor = yellow]
      if (not (any? yellow-ers))
      [ set isolated? true ]
    ]
  ; = = = = = = = = = =
  ask mushrooms
  [ let yellow-ers extended-root-set with [pcolor = yellow]
      ;; if mushrom root extended-root-set touches anything, it will have [pcolor = yellow]
      if (not (any? yellow-ers))
      [ set isolated? true ]
  ]
end

to hide-isolated-agents
  ;; don't want to kill the isolated agents, just hide them in the background
  let iso-agt all-agents-set with [isolated? = true]
  if (any? iso-agt) [
    ask iso-agt [
      ;; all-agts is spruce, fir, and mushroom breeds
      ;; they all have extended root sets
      ;; set everything to black - fading into background
      let ers extended-root-set
      ask ers [
        set color black
        set pcolor black
      ]
      set shape "default"
      set color black
      set pcolor black
    ]
  ]

end

;;----- Start of general utilities -----
;; Utility to generate a list of patches for spruce seeds
to rand-spruce-pxy-coord
  ask n-of num-spruce-seeds patches [
    loop [  ;; add a random seed patch to the front of list of seed patches
      set spruce-plist fput (patch random-pxcor random-pycor) spruce-plist
      stop ]
    ]
end

;; Utility to generate a list of patches for fir seeds
;; BUT not too close to the spruces
to rand-fir-pxy-coord
  let tree-patches spruce-plist
  let trees patch-set tree-patches
  let num-good-fir-seeds 0
  let separation-distance 4 * root-radius
  while [num-good-fir-seeds < num-fir-seeds]   ;; generate as many mushroom seeds as input slider requests
  [ let candidate-patch (patch random-pxcor random-pycor)
    ;; check candidate against known tree patches for distance violation
    ask candidate-patch [ let violators trees with [distance myself < separation-distance]
                          if not any? violators         ;; no trees violate distance check
                          [
                            set fir-plist fput candidate-patch fir-plist
                            set num-good-fir-seeds (num-good-fir-seeds + 1)
                          ] ;; use patch
    ] ;; close ask candidate patch
  ] ;; close while
end

;; Utility to generate a list of patches for mushroom seeds
;; BUT - not too close to the randomly placed spruces and firs patches
to rand-mushroom-pxy-coord
  let tree-patches sentence spruce-plist fir-plist  ;; check both kinds of trees
  let trees patch-set tree-patches
  let num-good-mush-seeds 0
  let separation-distance 3 * root-radius
  while [num-good-mush-seeds < num-mushroom-seeds]   ;; generate as many mushroom seeds as input slider requests
  [ let candidate-patch (patch random-pxcor random-pycor)
    ;; check candidate against known tree patches for distance violation
    ask candidate-patch [ let violators trees with [distance myself < separation-distance]
                          if not any? violators         ;; no trees violate distance check
                          [
                            set mushroom-plist fput candidate-patch mushroom-plist
                            set num-good-mush-seeds (num-good-mush-seeds + 1)
                          ] ;; use patch
    ] ;; close ask candidate patch
  ] ;; close while
end

;;+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
;; Because the random patch list generation is done in three different invocations,
;; there is concern that selection of a random patch by trees and mushrooms could
;; result in duplicates. (Duplicates don't seem to happening so comment out call to clean-plists)
;;
;; If duplicates occur in future, call this utility to clean up randomly generated plists
;; for {spruces and firs} and {mushrooms} [not perfect - needs more index control]
;; The strategy: First make one joined list by combining the two tree plists {spruces, firs}
;; Remove duplicates and decide tree breakpoint to separate them again (later) into {spruces, firs}
;; Repeat strategy in similar fashion for mushrooms joined to reduced tree list
;; Limitations: First reduced joined tree list length could shrink upon remove-duplicates,
;; but we keep the reduced joined tree list constant by subsuming mushroom patches
;; for the trees if necessary.
;; ALSO NOTE: For sublist index1 is inclusive while index2 is exclusive
to clean-plists
  ;; join the tree lists, de-dup, establish index for breaking joined list back into 2 lists
  let joined-plist sentence spruce-plist fir-plist   ;; {spruces, firs}
  set joined-plist remove-duplicates joined-plist
  let trees-plist-len length joined-plist  ;; preserve this length for trees part of list
  let trees-breakpoint trees-plist-len / 2  ;; for now, distribute equitably between tree breeds
  ;; join the mushroom list to the joined, de-duped tree list and de-dup again
  let joined-hybrid-plist sentence joined-plist mushroom-plist  ;; {spruces, firs, mushrooms}
  set joined-hybrid-plist remove-duplicates joined-hybrid-plist
  set spruce-plist sublist joined-hybrid-plist 0 (trees-breakpoint)
  set fir-plist sublist joined-hybrid-plist (trees-breakpoint) (trees-plist-len)
  set mushroom-plist sublist joined-hybrid-plist (trees-plist-len) (length joined-hybrid-plist)
end
;;+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +

;;------ end utilities ------

; Copyright 2006 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
261
10
874
644
100
100
3.0
1
10
1
1
1
0
0
0
1
-100
100
-100
100
1
1
1
ticks
30.0

BUTTON
20
195
83
228
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
155
195
218
228
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
440
235
473
tree-wiggle-angle
tree-wiggle-angle
10
360
60
5
1
degrees
HORIZONTAL

SLIDER
10
477
235
510
num-spruce-particles
num-spruce-particles
0
100
80
1
1
NIL
HORIZONTAL

SLIDER
11
513
236
546
num-fir-particles
num-fir-particles
0
100
80
1
1
NIL
HORIZONTAL

SLIDER
9
593
234
626
num-mushroom-particles
num-mushroom-particles
0
100
100
1
1
NIL
HORIZONTAL

SLIDER
9
556
234
589
mushroom-wiggle-angle
mushroom-wiggle-angle
10
360
120
5
1
degrees
HORIZONTAL

SLIDER
20
150
218
183
num-mushroom-seeds
num-mushroom-seeds
1
100
30
1
1
NIL
HORIZONTAL

SLIDER
19
66
219
99
num-spruce-seeds
num-spruce-seeds
1
50
20
1
1
NIL
HORIZONTAL

SLIDER
19
109
219
142
num-fir-seeds
num-fir-seeds
1
50
20
1
1
NIL
HORIZONTAL

MONITOR
902
23
1037
68
Spruce Mean Age
mean [age] of spruces
2
1
11

MONITOR
903
74
1038
119
Fir Mean Age
mean [age] of firs
2
1
11

MONITOR
905
125
1040
170
Mushroom Mean Age
mean [age] of mushrooms
2
1
11

TEXTBOX
20
355
170
420
Controls below tweak root growth \nfor trees and mushrooms IF diffusion used
12
0.0
1

TEXTBOX
24
12
174
57
How many spruce, fir, \nand mushrooms to generate?
12
0.0
1

TEXTBOX
915
450
1065
476
Controls below tweak run time limits for simulation
11
0.0
1

PLOT
905
180
1105
330
mushroom connections
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" "plot count mushroom-roots with [pcolor = yellow]"

SLIDER
915
490
1100
523
tree-pass-limit
tree-pass-limit
0
200
90
1
1
NIL
HORIZONTAL

SLIDER
915
530
1102
563
mushroom-pass-limit
mushroom-pass-limit
0
200
140
1
1
NIL
HORIZONTAL

SWITCH
915
400
1100
433
hide-isolated?
hide-isolated?
0
1
-1000

@#$#@#$#@
## ACKNOWLEDGMENT (Diffusion Model) - see additional acknowledgements at end

This model is from Chapter Three of the book "Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo", by Uri Wilensky & William Rand.

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

This model is in the IABM Textbook folder of the NetLogo Models Library. The model, as well as any updates to the model, can also be found on the textbook website: http://www.intro-to-abm.com/.

## WHAT IS IT?

Like the main DLA model, this model demonstrates diffusion-limited aggregation, in which particles moving (diffusing) in random trajectories stick together (aggregate) to form beautiful treelike branching fractal structures. There are many patterns found in nature that resemble the patterns produced by this model: crystals, coral, fungi, lightning, and so on.

This model is called DLA Simple because it is it is a simplified version of the main DLA model from the NetLogo models library. In the main model, new particles are created as existing particles aggregate. In this model, particles are only created at the beginning. The main model is more computationally efficient, but the rules that drive the phenomenon are more digestible in this model.

## HOW TO USE IT

Press SETUP to make the initial seed and NUM-PARTICLES particles, then press GO to run the model.
The WIGGLE-ANGLE slider controls how wiggly the paths the particles follow are. If WIGGLE-ANGLE is 0, they move in straight lines. If WIGGLE-ANGLE is 360, they move in a totally random direction at each time step.

## THINGS TO NOTICE

Note that the resulting structure has a branching structure, like a tree.  Why does this happen?

What other phenomena in the world do the shapes remind you of?  Is this aggregation process a plausible model of how those phenomena occur?

## THINGS TO TRY

Try different settings for how much the turtles turn as they do their random walk (the WIGGLE-ANGLE slider).  What is the effect on the appearance of the resulting aggregate?  Why?

Does it make any difference whether there are more or fewer particles?  Why or why not?

## EXTENDING THE MODEL

What happens if you start with more than one "seed" patch?  What happens if the seed is a line instead of a point?

Can you find a way to modify the code so the resulting pattern spirals out instead of radiating straight out?

The rule used in this model is that a particle "sticks" if any of the eight patches surrounding it are green.  What do the resulting structures look like if you use a different rule (for example, only testing the single patch ahead, or using `neighbors4` instead of `neighbors`)?

Can you compute the fractal dimension of the aggregate?

If instead of using green, you gradually vary the color of deposited particles over time, you can see more vividly the accretion of "layers" over time.  (The effect is also visually pleasing.)

The model will run faster if the turtles are invisible, so you may want to add a switch that hides them (using the HT command).

## NETLOGO FEATURES

Note the use of the `neighbors` primitive.

## RELATED MODELS

The various models in the "Fractals" subsection of the "Mathematics" section of the Models Library demonstrate some other ways of "growing" fractal structures.

The "Percolation" model in the "Earth Science" section produces patterns resembling the patterns in this model.

## CREDITS AND REFERENCES

This model is a simplified version of:

* Wilensky, U. (1997).  NetLogo DLA model.  http://ccl.northwestern.edu/netlogo/models/DLA.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

The concept of diffusion limited aggregation was invented by T.A. Witten and L.M. Sander in 1981.  Witten, T. & Sanders, L. (1981).  Diffusion-limited aggregation, a kinetic critical phenomena. Phys. Rev. Lett. 47(19), 1400–1403 (1981).

Tamas Viczek's book "Fractal Growth Phenomena" contains a discussion, as do many other books about fractals.

## HOW TO CITE

This model is part of the textbook, “Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo.”

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U., Rand, W. (2006).  NetLogo DLA Simple model.  http://ccl.northwestern.edu/netlogo/models/DLASimple.  Center for Connected Learning and Computer-Based Modeling, Northwestern Institute on Complex Systems, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the textbook as:

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

## COPYRIGHT AND LICENSE

Copyright 2006 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2006 Cite: Wilensky, U., Rand, W. -->


## ADDITIONAL ACKNOWLEDGMENT (Mushroom-Tree Network Model)
This model was developed as a project assignment for a Massive Open Online Course
(MOOC), An Introduction to Agent-Based Modeling, sponsored by the Complexity Explorer, through the Santa Fe Institute, and taught by Professor Bill Rand.

The inspiration for this model comes from a TED Talk by Suzanne Simard. See
https://www.ted.com/talks/suzanne_simard_how_trees_talk_to_each_other?language=en#t-5465

Teri Roberts developed the code to visually generate and map a forest-setting network
of spruce/fir trees amd mushrooms with root systems using the Diffusion Model to grow the roots. The roots are color-coded and define the connectivity between the types of plants.
After the root touching conditions (adjacency lists) are established, one line of code
produces the network graph. The network then provides a basis for perturbation of the
plant network by adding and removing trees and assessing results. Please forgive the
newbie fumbling with the code development and style. My goal was to learn the NetLogo language and Integrated Development Environment (IDE) tools and to share this code with the New Mexico Supercomputing Challenge as entertainment and enlightenment.
See http://www.supercomputingchallenge.org/
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

mushroom
false
0
Polygon -7500403 true true 150 150 105 255 195 255
Circle -7500403 true true 75 60 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
setup
repeat 450 [ go ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
