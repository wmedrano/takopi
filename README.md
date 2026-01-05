# Takopi

LLM-powered editing capabilities for Emacs.

## Overview

Takopi is an Emacs package that brings LLM (Large Language Model) assistance directly into your editing workflow. It allows you to use AI to resolve TODO comments, apply code changes, and manage editing sessions with full transparency.

## Features

- **TODOAI Resolution**: Automatically resolve `TODOAI` comments in your code using LLM assistance
- **Session Management**: Track multiple concurrent LLM editing sessions
- **Visual Feedback**: Mode line indicator showing active sessions and thinking status
- **Session Viewer**: Inspect full chat history and applied changes in org-mode format
- **Configurable Thinking Levels**: Control LLM reasoning depth (none, light, medium, maximum)
- **Diff-based Editing**: LLM applies precise diff-based changes to your buffers

## Installation

### Prerequisites

Takopi requires the `llm` package for LLM integration. Install it through your preferred Emacs package manager.

### Setup

1. Clone or download this repository
2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/takopi")
(require 'takopi)

;; Configure your LLM provider (example with Gemini)
(require 'llm-gemini)
(setq takopi-llm-provider
      (make-llm-gemini :key "your-api-key"))

;; Enable status mode for mode line indicator
(takopi-status-mode 1)
```

## Usage

### Resolving TODOAI Comments

Add `TODOAI` comments in your code where you want LLM assistance:

```python
# TODOAI: Implement binary search algorithm with proper edge case handling
def search(arr, target):
    pass
```

Then run:
- `M-x takopi-todo` - Process all TODOAI items in the current buffer

The LLM will locate each `TODOAI` comment and implement the requested functionality while maintaining your code style.

### Managing Sessions

- `M-x takopi-show-session` - View details of a session (chat history, tools, changes)
- `M-x takopi-session-cancel` - Cancel an active session
- `M-x takopi-session-cancel-all` - Cancel all active sessions
- `M-x takopi-session-clear` - Clear all sessions
- `M-x takopi-status-report` - Display count of active sessions

### Configuring Thinking Level

Control how much reasoning the LLM applies:

```elisp
M-x takopi-session-set-thinking
```

Options:
- `none` - No extended reasoning
- `light` - Minimal reasoning (25%)
- `medium` - Moderate reasoning (50%)
- `maximum` - Full reasoning (100%)

## Mode Line Indicator

When `takopi-status-mode` is enabled, the mode line shows:
- `🐙[N]` - N active sessions, idle
- `🐙[N:🧠XX%]` - N active sessions, thinking at XX% intensity

## Session Viewer

The session viewer displays:
- **Task**: Description of what the session is working on
- **Chat History**: Full conversation between you and the LLM
- **Tools**: Available tools the LLM can use
- All content is formatted in org-mode for easy reading

## Architecture

Takopi consists of three main components:

- **takopi.el**: Main entry point, status mode, and high-level commands
- **takopi-session.el**: Session management, storage, and rendering

The package uses the `llm` library to communicate with various LLM providers (Gemini, OpenAI, Claude, etc.).

## Version

Current version: 0.1.0

## Author

Will S. Medrano

## License

See LICENSE file for details.
