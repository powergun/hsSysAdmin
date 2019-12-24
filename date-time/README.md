# Date & Time

inspired by: hsInterview/da

## Data.Time.Calendar

### How to use the latest version 1.9.0 with stack

this is a common issue seen in other places (package xxx from stack
configuration does not match yyy)

enable a project-specific configuration entry, called `allow-newer: true` see: <https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config>

this is needed at the time of writing as stack locks the (latest) version to
1.8.0.3

**however it turns out that 1.8.0.3 is sufficient to solve the challenge in
hsInterview/da**

### Tutorials

```text
https://two-wrongs.com/haskell-time-library-tutorial
```

### Case Example: Age restriction

Problem: to register a user, a candidate must be X year old
(X is 20 in the example)

This uses some important concepts of `Data.Time.Calendar` and
`Data.Time.Clock` libaries
