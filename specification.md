_This spec is __not__ complete. It describes the minimal viable app. It is inspired by [Joel on Software's introduction to functional specs](https://www.joelonsoftware.com/2000/10/02/painless-functional-specifications-part-1-why-bother/)._

# Overview
> Engage, divide, and conquer your tasks with Breakdown Todo.

Everything is a task. Each task can be broken down into smaller tasks. You are always presented with an actionable next step.

Tasks may have relationships. They can depend on the completion of others, or be grouped into a higher-level goal.

## Non-goals

- Projects/Groups of tasks. Projects are not actionable.
- Strict enforcement of opinion. Rules should be encouraged, but breakable.

# Scenarios
The scenarios serve to drive the development on the basis of use cases. They should be plausible and showcase real needs that will be addressed with the system.

## Capture
Peter is always busy. If he doesn't sleep, he showers, cooks, eats, works, or goes to bed. In his busy life, he often remembers things to do at the wrong time. "Buy milk" at work, "Fix the bike's lock" in the shower, "Fix that bug" during breakfast. Of course, when it is time to actually do these things, he will already have forgotten them.

With Breakdown, Peter can simply input whatever task pops into his head into his phone. If he remembers that he should "Set the alarm earlier" during cooking, he can quickly clean his greasy hands, open Breakdown on his phone, tap in "Set alarm" and save the task.

Since the list of current tasks is on his phone's homescreen, every time he unlocks his phone, he has a chance of seeing it. That is why, when browsing reddit while going to bed (he knows screens aren't good late at night, fuck off), he notices the list, decides to quickly check it for anything he had missed during the day, and sees the reminder.

What a good thing he noticed, or else he might have slept through his marriage day...

## Planning
Break a task into subtasks.

## Delayed Planning
Add a duration separately from capture with reminder.

## Future Reminder
Defer a task until a date.

## Dependency
Defer a task until completion of another.

# Activities
## Engage
Input of new tasks must be fast. Whether the user will subsequently move on to divide the task depends on his situation. Breakdown Todo supports both capture of ideas to be dealt with later, as well as planning of ideas to be divided immediately.

At this stage, only the task's action is needed. An action always ends with an exclamation point! This encourages to phrase it an actionable way.

Optionally, a duration may be specified. By not specifying it during capture, the user may suggest that they want to plan this task later. In case the duration is specified as well, the divide activity opens.

## Divide
A fully specified task has a duration. Tasks for which no duration has been specified are considered incomplete. The user is encouraged to complete these tasks.

### Subtasks
Users are encouraged to divide the task into an ordered list of subtasks, if the action isn't immediately executable.

The parent task's action is the header, reminding the user of the ultimate goal. The first subtask's action becomes the parent task's action.

Subtasks are assumed to depend on the previous subtask, in the sense that all subtasks after the first are considered deferred.

### Deferral
If the user decides to defer a task, they will enter a question with which they will be prompted on the chosen date. This makes the condition on which the deferral is based explicit. Deferral should be somewhat tedious, to discourage unnecessary deferral.

Deferred tasks do not show up in the list of things to do next. If a deferred task is shown, instead of the action, the deferral question is shown, to remind the user of what this task is waiting for.

A task can be deferred until another task (or multiple tasks) are done.

## Conquer
Conquering a task should be rewarded with a pleasing, swift animation.

Optionally, the user may track the time they took to accomplish the task. For this they start the task and later conquer it. They receive feedback on whether they stayed inside the anticipated duration.
