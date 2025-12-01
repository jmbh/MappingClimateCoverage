"""Provides a function `make_prompt` to create LLM input for a single article,
consisting of a title, content, and synopsis.
If this file is run directly, it will print an example prompt.
Run as `python3 Prompting/prompting.py`.
"""

import json
from prompt import PROMPT

summary_question = "Summarize the article in at most 100 words."

QUESTIONS = "Prompting/questions.json"
with open(QUESTIONS) as questions_file:
    QUESTIONS = json.load(questions_file)


def make_prompt(title=None, content=None, synopsis=None):
    global QUESTIONS, PROMPTS
    if title is None or content is None:
        raise ValueError("Missing fields!")
    question_identifiers = list(QUESTIONS.keys())
    questions = "\n".join(
        f"[{ident}]: {QUESTIONS[ident]['text']}" \
            for ident in question_identifiers
    )
    if synopsis:
        synopsis = f"Synopsis: {synopsis}\n\n"
    else:
        synopsis = ""
    prompt = PROMPT.format(
        questions = questions,
        title=title.strip(),
        content=content,
        synopsis=synopsis,
        example=""
    )
    return prompt

if __name__ == "__main__":
    print(make_prompt(title="TITLE", content="CONTENT", synopsis="SYNOPSIS"))