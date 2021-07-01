# TerminusDB Schema

The Terminus schemas are a self-documenting, formal description of all of the internal datatypes and classes used by the TerminusDB engine. They cover such aspects as internal configuration; documents; relationships; time, space and geo-temporal scoping of data and values; annotations and provenance; and a range of basic building blocks such as some useful datatypes and classes.

On top of these core schemas, the TerminusDB schemas define the governance structure of the database itself - they form the schema of the capability database that governs the system. It's schemas all the way down - every part of the server's configuration and saved state has a schema managed by the system, even the capability database. We eat our own dogfood. This makes a lot of sense because we get user-interfaces and forms for free if we use our own system. Using schemas to describe the internal datatypes and structures allows us to much more easily extend and improve our system as we learn more.

The specific schema files and how they are used by TerminusDB are detailed below.

## System Schema - Terminus Configuration Schema
system_schema.json

Contains classes to describe and configure, Agent, User, Role, Capability, Resource, Server, Database, their properties and the relationships between them.

## Repository
repository.json

Contains information about the metadata associated with a repository. This includes the head of the local or remote repositories, and their name and in the case of remote repositories,
their URL.

## Ref
ref.json

Contains the schema relating to commits, branches, tags etc.

## WOQL
woql.json

Contains the full WOQL query language. This allows queries to be saved
and retrevied from the database.
