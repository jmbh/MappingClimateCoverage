"""Prompt template. Questions are static, example is empty."""

PROMPT = (
"Read the following article from a German newspaper "
"and answer the questions at the end as accurately as possible.\n"
'''
"""Title: {title}

{synopsis}{content}"""

'''
"Your response is a JSON "
"that maps question identifiers (without brackets) to lists of two elements. "
"The first entry is your answer. "
"It is always a single number (or null), or a string. "
"The second entry is always a one-sentence explanation of the answer "
"given in the first list entry.\n"
'If the question is stated in terms of "mention or imply", '
"answer with one of: "
'{{0="none", 1="weakly implied", 2="strongly implied", 3="mentioned"}}.\n'
"If not specified otherwise, answer each question with one of: "
'"no" or "yes".\n'
'''
Questions:
{questions}
{example}'''
)