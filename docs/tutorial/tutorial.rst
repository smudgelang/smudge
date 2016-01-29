=================
Turnstyle Example
=================

.. contents::

.. sectnum::

Simple Turnstyle
================

In this tutorial, we'll be creating a simple turnstyle program. It
will accept coin events, unlock itself, let a person walk through, and
lock itself. Here's a nice drawing of the basic state machine:

.. figure:: 1/turnstyle.png
    :width: 300

The source for this state machine is available in the
1/turnstyle.smudge. You can see the essential elements of a smudge
state machine. The word _turnstyle_ is the name of the state
machine. The part inside the curly braces after the state machine's
name is its definition.

Inside the state machine's definition, there are one or more states
separated by commas. The turnstyle example has 2 states called
*locked* and *unlocked*. The initial state will be *locked* for this
state machine.

Each state has the events (also comma separated) that it handles
listed as transitions. A transition is, syntactically, an event name
followed by an arrow followed by a state name. The event causes the
state machine to go to the named state. In this example, the events
are **coin** and **person**. Note that not every event is handled by
every state. Smudge will generate an error if **coin** is sent to a
state machine that's in *unlocked*.

:: 

    Caveat!
    Smudge version 0.2 is a work in progress. The next section will assume
    that smudge is run with the --c-no-debug flag because that's what
    works without modifications right now.

Build it
--------

So now that we have some Smudge code, let's try building it and making
a program that works. First, run Smudge on the example.

::

   smudge 1/turnstyle.smudge

This will generate some dot output (the picture above), a C file and a
pair of headers. They all get dropped in the same directory as the
.smudge file (1/).

Use it
------

To use the generated C code, make a main.c file that defines the
assert function whose prototype is in turnstyle_ext.h.

Now you can send the coin and person events by calling the appropriate
event functions. Pass NULL as the argument for now, we'll discuss what
it does later.

Do Something
============

So far, we have a state machine that can transition states, but it
doesn't do anything. For that, we need side effects. There are two
types of side effects in Smudge: C functions that are called directly
(called @functions) and events. For now, we'll focus on @functions.

First of all, their names can be any valid C identifiers for reasons
that will become apparent later.

When a person walks through an unlocked turnstyle, we'd like it to
play a sound. When a coin is inserted, it should flash some LEDs. To
add these features to our turnstyle state machine, see
2/turnstyle.smudge.

Note that the arrows have changed from --> to -(@function)->. This is
a more general arrow. The full arrow syntax is **-(** followed by a
comma separated list of side effects (@functions and events) followed
by **)->**. The **-->** syntax we've been using is shorthand for
**-()->**.

Now run Smudge again and look at turnstyle_ext.h. Note that in
addition to assert, there are prototypes for flashLEDs and
soundOkay. These functions have to be added to main.c.

Event Payloads
==============

You may have noticed that these side effect and event trigger
functions take pointers to arguments whose types are left incomplete
in turnstyle.h. Smudge won't ever put anything in these payloads, but
you can. Let's say that you want to pay attention to who's going
through your turnstyle and play a nice customized greeting to them
when they pass. Example 3 doesn't change the Smudge code at all, but
adds some code to main.c to do just that.

This isn't a tutorial on C, but it's worth pointing out that you need
to be particularly careful with how you manage this memory. None of it
is done for you by Smudge and it's very easy to develop memory leaks
if you dynamically allocate space for a message then forget to free it
in one of the possible event handlers. This is also a good time to say
that side effects will always be called in the order they're listed in
the Smudge code.

Enter/Exit Functions
====================

Now our turnstyle can accept a coin and allow a person through. It
doesn't actually lock or unlock though. To do that, we need to call
side effect functions when we enter the locked and unlocked
states. Between the name of a state machine and the [, there is an
optional list of side effects surrounded by parentheses. These can be
@functions or events just like in arrows. Likewise, there's an
optional parenthesized list of @functions after the ].

Unlike event side effects, these @functions don't accept any
arguments. If you use the same function name as an enter/exit function
as an event side effect, the prototype for the function will accept no
arguments and the event won't be passed to the side effect function.

Example 4 adds lockedEnter and lockedExit as well as unlockedEnter to
the state machine. Now the turnstyle can actually lock and unlock
itself instead of just waving as people go through.

Transitionless Events
=====================

Our turnstyle is starting to look pretty nice, but what if a person
tries to go through it without paying? It would be nice to have an
event that's handled by a state, but that doesn't cause a state
transition. We could put **person --> locked** in the locked state,
but that would cause it to exit and re-enter locked just because a
person tried to jump through. Since we don't want @lockedExit or
@lockedExit called, we can use a different kind of event handler.

In addition to the arrow syntax, Smudge supports dash syntax to handle
an event without a state transition. It looks like **-(<side effect
list>)-**. Like with arrows, if the side effect list is empty you can omit
the parentheses.

Example 5 adds a second event handler (note the comma) to the locked
state and a new @function called soundAlarm.

Transient States
================

Great, now our turnstyle shames people who try to get through without
paying. Let's add a little state to power it up. Instead of starting
in locked, it should light up all its LEDs in a test pattern then go
straight to locked. This new state is called a transient state because
it can't ever get any events.

Example 6 adds this little state and a message to indicate that the
turnstyle is powering up.

Multiple State Machines
=======================

The turnstyle has been running nicely for a few days now, and the
customer (a subway system) is very happy. Wait a second, they say
they've been getting less money than expected and metal slugs are
piling up in the coin bin! That's no good, we need to validate those
coins before accepting them!

Smudge allows multiple state machines to be defined in the same
file. Example 7 adds a new simple single-state machine to validate
coins and deal with fakes. This is quite a bit of new code, but the
only really new piece of syntax is sending an event as a side effect
to a different state machine.

The C code here gets a little dicey, since the two state machines
really should be running in parallel with each other. However, since
this isn't a C tutorial, it will suffice.

Default States
==============

What if we want to handle a particular event the same regardless of
the current state? If someone shakes the turnstyle, it should give off
a warning regardless of the current state of the machine. We could put
**tilt -(@soundAlarm)-** in every state, but that's error prone and
nightmarish to maintain. Instead, there's a special state called the
*any-state*. Its name is a single underscore (**_**). If an event is
not specified in the current state, but it is specified in the
any-state, it will be handled according to its handler in the
any-state. An event that's specified in both will be handled by the
current state.

Example 8 shows some extra code to handle tilting. Note that the
soundAlarm @function has had its signature changed because it's called
in two incompatible contexts. In this example, tilting the machine
will cause it to eat any money that's been inserted. They deserve it.

Default Events
==============

Up until now, an unhandled event will cause the state machine to quit
with an error ("Game Over!"). That's not very friendly. If we have a
state where we don't want to crash on events that aren't handled
explicitly, but want to take the same action on all of them, we can
use the *any-event*.

Like the any-state, the any-event's name is a single underscore
character. Events not explicitly handled by a state will be handled by
the any-event. Example 9 shows the any-event in action. Since event
handlers in states take priority over the any-state, we need to
explicitly handle all the events from the any-state in a state with
the any-event.
