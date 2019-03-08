_This spec is __not__ complete. It describes the minimal viable app. It is inspired by [Joel on Software's introduction to functional specs](https://www.joelonsoftware.com/2000/10/02/painless-functional-specifications-part-1-why-bother/)._

# Overview
> Engage, divide, and conquer your tasks with Breakdown Todo.

Everything is a task. Each task can be broken down into smaller tasks. You are always presented with an actionable next step.

## Non-goals
- Projects/Groups of tasks. Projects are not actionable.
- Strict enforcement of opinion. Rules should be encouraged, but breakable.

# Scenarios
The scenarios serve as grounded justification for features. They should be authentic and describe problems that are solved by Breakdown.

## Planning
Anna, an ambitious student, has a problem any student has; procrastination. She will go to extreme lengths to avoid starting work on any task. Her room always is tidiest during exam phase.

But she knows that once she starts working on something, she usually manages to accomplish a fair amount of work. Only getting started is the problem.

Most of the time, her Todo list has quite abstract tasks on it. "Do homework", "Study machine learning", "Clean the room". It's more a list of problems than a list of things to do, therefore she feels overwhelmed and does not want to even start a task. The tasks are not actionable enough to motivate Anna to start doing them.

When Anna has a task at hand that she is not entirely motivated to do, she opens Breakdown and starts breaking it down into smaller tasks. "Clean the room" consists of

- Sort stuff on desk (5 min)
- Throw away garbage into bins (3 min)
- Put clothes in basket (2 min)
- Vacuum floor (7 min)
- Sweep floor

She is unsure how much time sweeping the floor will take, so she breaks that subtask down further:

- Fill bucket with water and soap (3 min)
- Sweep floor (7 min)
- Throw away water (2 min)
- Put away bucket and mop (2 min)

With this, the total estimated time for cleaning her room is 31 minutes. The first thing she needs to do is "Sort stuff on desk". That only takes 5 minutes. And she could stop anytime, if she doesn't feel like continuing.

As often is the case, once she started, she soon is cleaning the entire appartement.

Studying for the exam will have to wait until later. At least 31 minutes.

## Capture
Frank is both super-busy and ambitious. Often during one task he will have ideas about what else he could do. But those new ideas need further consideration than Frank is willing to spend right now, because his cat is demanding full petting-attention.

Therefore, Frank quickly notes down the general idea, "Cook cheesecake", without setting a duration. This will file the idea as an "unplanned" one, such that Frank can easily come back to it later, after he has enough of his cat's purring.

Before Frank has had enough of his cat, the cat decides that it doesn't like being pet anymore, stands up and walks away arrogantly. While checking his phone, Frank sees he has unplanned tasks, so he decides to revisit it.

Since "Cook cheesecake" is a bit too complex to estimate the duration, he breaks it down into subtasks. He then obtains a delicious estimate that motivates him to fire up the oven.

Unfortunately, the cake tastes weird, slightly burnt. Frank investigates and discovers that his cat had decided to take a nap in the oven...

## Future Reminder
Barry likes to plan ahead. He often thinks of tasks that he will have to accomplish later. Even though he tries to do everything as soon as possible, sometimes he has to wait before he can start working on something.

He recently booked a flight to Ireland. Barry knows that he will have to check in online. Unfortunately, the flight company does not open the check in until three days prior to the departure. So Barry cannot do it know, he has to wait another two weeks.

Barry does not want to forget checking in, so he creates a new task, "Check in online", and defers it until three days prior to the departure. The task does not show up in the current ones.

Three days prior to the departure, Breakdown reminds him about the check in. Barry goes online and manages to check in, so he marks the task as done.

Unfortunately, Barry packed a metal spoon in his luggage. He is now considered a terrorist and will have to spend three months in jail.

## Wait for Answer
John works at the support for a university's online system. He gets constantly bombarded with emails, because the system is poorly designed and students need help for the smallest of tasks.

Because John is a good man, he works on finding design improvements that he then suggests to his superior. Of course, his superior is always busy, bathing in the money his company gained from the contract with the university. So John has given up on expecting timely answers. Instead, he has to regularly reiterate his suggestions, before they all ultimately are denied (hope dies last).

Again, John has identified that many students have trouble finding their enrollment attestation. After adding "Make attestations easier to find", he writes up an email proposing to make that functionality more prominent on the landing page while reducing its clutter. Since he is certain that he will have to resend a mail, he adds a subtask to "Check mail" and defers that until next Wednesday. (He is really patient and gives his superior a grace period of a few days in which to ignore John.)

This time, the superior will answer before the deadline given by John, because he spent all his money and has to fire John.

# Activities
## Engage
Input of new tasks must be fast. Whether the user will subsequently move on to divide the task depends on his situation. Breakdown Todo supports both capture of ideas to be dealt with later, as well as planning of ideas to be divided immediately.

At this stage, only the task's action is needed. An action always ends with an exclamation point! This encourages to phrase it an actionable way, i. e. "Write mail" instead of "Email".

Optionally, a duration may be specified. By not specifying it during capture, the user suggests that they want to plan this task later. In case the duration is specified as well, the divide activity opens.

## Divide
A fully specified task has a duration. Tasks for which no duration has been specified are considered incomplete. The user is encouraged to complete these tasks.

### Subtasks
Users are encouraged to divide the task into an ordered list of subtasks, if the parent action itself is too broad and not actionable.

The parent tasks summary contains both the parent action, to remind the user of the ultimate goal, and the first subtask's action, in order to provide an immediately actionable task.

### Deferral
If the user decides to defer a task, they chose a date on which the task will become actionable. Deferral should be somewhat tedious, to discourage unnecessary deferral, e. g. by prompting for a question that describes to reason to defer.

Deferred tasks do not show up in the list of things to do next. Deferred task are ordered and grouped by their date.

## Conquer
Conquering a task should be rewarded with a pleasing, swift animation.

Optionally, the user may track the time they took to accomplish the task. For this, a Pomodoro-style timer is provided, where each session can be attributed to a task (as does TickTick).
