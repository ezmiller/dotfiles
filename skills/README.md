# Skills Repository

This repository contains a collection of agent skills following the [AgentSkills specification](https://agentskills.io/spec). Each skill is a machine-readable definition for a modular capability that can be leveraged by AI agents and LLM-powered tools.

## About Skills

Skills are a new and evolving standard for agent capabilities. This repository houses custom skills for various use cases.

## Usage

These skills can be used with various agent frameworks and AI assistants:

- **Claude:**
  Symlink this skills directory into your `.claude` folder:
  ```bash
  ln -s ~/Projects/skills ~/.claude/skills
  ```

- **Other Tools:**
  Check your agent/tool documentation for instructions on skill directory integration.

## Skills in this Repository

- **financial-aid-application** - In development - Guides users through financial aid application processes
- **farsika-server** - Manage, troubleshoot, and answer questions about the farsika personal server (S3 backups, Resilio Sync, Tailscale)

## Contributing

This is a personal skills repository. Skills are developed based on real-world use cases and refined over time.
