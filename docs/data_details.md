# Comprehensive Analysis of CPSC Experience Survey Data

## 1. Data Structure

### File Overview

**File:** [`survey_data/CPSC Experience Survey.csv`](survey_data/CPSC Experience Survey.csv)

### Column Count

The CSV contains 37 columns with the following structure:

| Column | Name | Data Type | Description |
|--------|------|-----------|-------------|
| 1 | Timestamp | String | Date/time of response (format: "YYYY/MM/DD h:mm:ss AM/PM EST") |
| 2 | What section are you in? | Categorical | Course section identifier (e.g., "231 - 1pm", "217 - 3pm") |
| 3 | Prior to taking this course, what was your programming experience? | Categorical | Programming experience level |
| 4 | How is the course meeting your expectations for what you hoped to learn or experience? (Optional) | Free Text | Open-ended response about course expectations |
| 5 | Do you prefer in-person or online learning? | Categorical | Learning preference |
| 6 | Why is this your preferred way of learning? | Free Text | Explanation of preference |
| 7 | (Optional) If you're not taking this class in your preferred learning method, why? | Free Text | Reason for not having preferred method |
| 8 | How much do you agree with the statement? [The content is relevant and up-to-date] | Likert Scale | Agreement statement (1-5 scale with text labels) |
| 9 | How much do you agree with the statement? [I am excited about the content and material that I'm learning] | Likert Scale | Agreement statement (1-5 scale with text labels) |
| 10 | How much do you agree with the statement? [I'm satisfied with the level of feedback I receive] | Likert Scale | Agreement statement (1-5 scale with text labels) |
| 11 | How much do you agree with the statement? [I feel like I could take what I'm learning and apply it in a new scenario] | Likert Scale | Agreement statement (1-5 scale with text labels) |
| 12 | How much do you agree with the statement? [It's easy to ask for help] | Likert Scale | Agreement statement (1-5 scale with text labels) |
| 13 | How much do you agree with the statement? [I feel like I am meeting the goals of learning Python in this course] | Likert Scale | Agreement statement (1-5 scale with text labels) |
| 14 | How much do the following elements contribute to your learning? [Explanations of pre-written code] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 15 | How much do the following elements contribute to your learning? [Studying for midterms] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 16 | How much do the following elements contribute to your learning? [TopHat Quizzes] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 17 | How much do the following elements contribute to your learning? [Presentation slides] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 18 | How much do the following elements contribute to your learning? [Post-class handouts and notes] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 19 | How much do the following elements contribute to your learning? [Coding on my own] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 20 | How much do the following elements contribute to your learning? [Live coding by the professor] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 21 | How much do the following elements contribute to your learning? [Labs] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 22 | How much do the following elements contribute to your learning? [Being able to ask questions of the professor during lecture] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 23 | How much do the following elements contribute to your learning? [Assignments] | Likert Scale | Learning element rating (1-5 scale with text labels) |
| 24 | Thinking about what helps you learn the best, if you are going to continue taking programming classes after this one: What do you wish the courses would do more of? And also, what do you wish they would do less of? (Optional) | Free Text | Multi-line question about course improvements |
| 25 | What's been your favorite part of the class for you so far, and why? (Optional) | Free Text | Favorite class experience |
| 26 | What's been the least enjoyable part of class for you so far, and why? (Optional) | Free Text | Least enjoyable class experience |
| 27 | How much do you agree with the following statements? [I feel comfortable speaking up in class] | Likert Scale | Community/belonging statement (1-5 scale with text labels) |
| 28 | How much do you agree with the following statements? [I feel like I am a part of this class] | Likert Scale | Community/belonging statement (1-5 scale with text labels) |
| 29 | How much do you agree with the following statements? [Making friends within the class is important to me] | Likert Scale | Community/belonging statement (1-5 scale with text labels) |
| 30 | How much do you agree with the following statements? [I feel like I am a part of the university community] | Likert Scale | Community/belonging statement (1-5 scale with text labels) |
| 31 | How much do you agree with the following statements? [It's easy to meet new people within the class] | Likert Scale | Community/belonging statement (1-5 scale with text labels) |
| 32 | What's the greatest challenge in meeting new people at university? (Optional) | Free Text | Social challenges |
| 33 | About the class Discord (select all that apply) | Multi-select | Discord usage (semicolon-separated values) |
| 34 | Please remark on aspects of the class that make it welcoming and inclusive to you, given your identities and needs, and suggest any aspects that could improve inclusive teaching in this class (Optional) | Free Text | Inclusivity feedback |
| 35 | What were your expectations/hopes for interacting with the other students? If it isn't meeting your wishes, we'd like to hear more. (Optional) | Free Text | Student interaction expectations |
| 36 | What were your expectations/hopes for interacting with the professor? If it isn't meeting your wishes, we'd like to hear more. (Optional) | Free Text | Professor interaction expectations |
| 37 | Any other comments that you would like to share that you feel would make the class more interesting or engaging for you? (Optional) | Free Text | General feedback |

## 2. Column Formats

### Question Response Formats

#### Likert-style Scale Responses (Columns 8-13, 14-23, 27-31)

**Format:** Numeric value with descriptive text label

- 1 - Strongly Disagree
- 1 - Doesn't contribute to my learning (for learning contribution options)
- 1- Not at all (for community statements)
- 2 (numeric only, no label)
- 3 (numeric only, no label)
- 4 (numeric only, no label)
- 5 - Strongly Agree
- 5 - Very much so (for community statements)
- 5 - Essential to my learning (for learning contribution options)

**Observation:** Inconsistent formatting - some responses include full text labels while others are just numbers.

This is easy to address, just need to strip non-numeric characters from the response, and then we are left with just the rating.

#### Categorical Values

**Section Identifiers (Column 2):**

- **Format:** "COURSE_NUMBER - TIME" (e.g., "231 - 1pm", "217 - 3pm")
- **Values observed:** "231 - 1pm", "231 - 11am", "231 - 3pm", "217 - 1pm", "217 - 11am", "217 - 3pm"
- Some entries are empty (null)

**Programming Experience (Column 3):**

- "Highly experienced (comfortable writing own programs)"
- "Took programming course before (either in school, or online tutorials)"
- "No experience at all"

**Learning Preference (Column 5):**

- "In-person"
- "Online"
- "No preference"

**Multi-Select Values (Column 33 - Discord)**

- **Format:** Semicolon-separated values
- **Example:** "I have joined the class Discord;I am active in the class Discord;It is really useful for me for learning"
- **Note:** Students could also provide their own answer in addition to the provided answers. Their answer is also part of the overall response string, separated by semicolon.

**Free Text Responses**

- Variable length (empty to multiple paragraphs)
- Can contain line breaks and special characters
- Some responses include bullet points or numbered lists
- May contain quotes (escaped with double quotes in CSV)

## 3. Data Organization

### Question Groupings

The survey is organized into logical sections:

#### Context (Columns 1-3)

- Timestamp, Section, Programming Experience

#### Course Expectations & Learning Preferences (Columns 4-7)

- Expectations, Learning preference, Reasoning

#### Course Content Agreement Statements (Columns 8-13)

- 6 statements about course relevance, excitement, feedback, applicability, help-seeking, goal achievement

#### Learning Elements Contribution (Columns 14-23)

- 10 elements: pre-written code explanations, midterms, TopHat quizzes, slides, handouts, coding on own, live coding, labs, asking questions of professor during lecture, assignments

#### Course Experience Feedback (Columns 24-26)

- Multi-line question about improvements, favorite/least enjoyable parts

#### Community & Belonging Statements (Columns 27-31)

- 5 statements about comfort speaking up, belonging, making friends, university community, meeting people

#### Social Challenges (Column 32)

- Challenges in meeting people

#### Class Discord Usage (Column 33)

- Class Discord multi-select

#### Inclusivity & Interaction Feedback (Columns 34-37)

- Inclusivity remarks, student interaction expectations, professor interaction expectations, general comments

### Participant Identifiers

- Conducted as an anonymous survey
- No explicit participant ID column
- Responses are identified only by:
  - Timestamp (may not be unique if multiple responses at same time)
  - Section identifier
- Each row contains the answers from an individual student

### Optional vs Required Questions

**Marked as Optional in column names:**

- Column 4: "How is the course meeting your expectations... (Optional)"
- Column 7: "(Optional) If you're not taking this class in your preferred learning method, why?"
- Column 24: Multi-part question (Optional)
- Column 25: "What's been your favorite part... (Optional)"
- Column 26: "What's been the least enjoyable part... (Optional)"
- Column 32: "What's the greatest challenge... (Optional)"
- Column 34: "Please remark on aspects... (Optional)"
- Column 35: "What were your expectations/hopes... (Optional)"
- Column 36: "What were your expectations/hopes... (Optional)"
- Column 37: "Any other comments... (Optional)"

**Likely Required (no Optional marker):**

- Section (Column 2) - though some entries are empty
- Programming experience (Column 3)
- Learning preference (Column 5)
- Why is this your preferred way of learning? (Column 6) - only 2/667 empty responses
- All Likert scale questions

### Potential Processing Challenges

- **Multi-select Parsing:** Discord responses need to be split by semicolons and analyzed as separate categorical variables.

- **Empty Values:** Several columns have empty/null values that need to be handled appropriately (e.g., Section column has some empty entries).

- **Free Text Analysis:** The free text columns contain variable-length responses that may require:
  - Text cleaning (removing extra whitespace, handling line breaks)
  - Handling of special characters and escaped quotes

- **Section Identifier Parsing:** Section identifiers combine course number and time, which may need to be split for certain analyses.

- **Duplicate Timestamps:** Multiple responses may have identical timestamps, making timestamp-based deduplication unreliable.

- **CSV Quoting:** Some free text responses contain quotes and line breaks, requiring proper CSV parsing to avoid data corruption.

### Recommendations for Processing

- Use a robust CSV parser that handles multi-line values and escaped quotes correctly.

- For Likert-type responses, simply strip all non-numeric characters to get the number

- Parse multi-select values into binary columns for each option.

- Generate synthetic participant IDs based on timestamp + section + a sequence number to make referencing individual reponses easier

- Clean free text by removing extra whitespace, normalizing line breaks, and handling special characters.

- Split section identifiers into separate columns for course number and time. Also keep original column that has both course and time as originally selected.
