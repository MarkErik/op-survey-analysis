# User Experience Documentation (Version 2)

## CPSC Experience Survey Explorer - Shiny Application

This document describes the user-facing features and capabilities of the Shiny survey analysis application. It focuses purely on what users can do with the app, without reference to implementation details.

---

## 1. Application Overview

The application is a comprehensive survey data explorer that allows instructors and teaching staff to analyze student responses to a course experience survey. It provides both aggregate visualizations and individual response browsing capabilities, along with advanced statistical analysis tools.

**Target Users:**
- Course instructors
- Teaching assistants
- Department administrators
- Educational researchers

---

## 2. Navigation Structure

The application uses a multi-tab navigation system:

### 2.1 Home Tab
The default landing page that displays aggregate statistics and visualizations.

### 2.2 Question Responses Tab
A dedicated view for browsing and exploring individual free-text responses.

### 2.3 Statistics Tab
A dedicated view for detailed statistical analysis of Likert-scale questions.

### 2.4 Insights Tab
A dedicated view for advanced statistical insights and correlations.

Users can switch between tabs using the tabset panel in the main content area.

---

## 3. Home Tab Features

### 3.1 Response Overview Section

**Total Responses Counter**
- Displays the total number of survey responses in the dataset
- Updates dynamically when filters are applied

**Section Breakdown Chart**
- Interactive bar chart showing response distribution across course sections
- Sections are organized by course number (217, 231) and time slot (11am, 1pm, 3pm)
- Users can click on any bar to filter all data to that specific section
- When a section is selected, the chart title updates to reflect the filtered view

**Section Filter Display**
- Shows which section is currently selected
- Includes a "Reset Filter" button to clear section selection and return to viewing all sections

### 3.2 Overview Visualizations

The Home tab displays six visualization panels, each showing different aspects of the survey data:

#### 3.2.1 Learning Preference Distribution
- Bar chart showing how students prefer to learn (In-person, Online, or No preference)
- Color-coded by preference type

#### 3.2.2 Prior Programming Experience
- Horizontal bar chart showing students' prior programming background
- Three categories:
  - No experience at all
  - Took programming course before (school or online tutorials)
  - Highly experienced (comfortable writing own programs)

#### 3.2.3 Course Satisfaction Overview
- Horizontal bar chart showing average satisfaction scores (1-5 scale)
- Covers 6 statements about course quality:
  - Content relevance and currency
  - Excitement about material
  - Satisfaction with feedback
  - Ability to apply learning to new scenarios
  - Ease of asking for help
  - Goal achievement in learning Python

#### 3.2.4 Discord Engagement Metrics
- Horizontal bar chart showing Discord usage and usefulness
- Three metrics:
  - Percentage who have joined the class Discord
  - Percentage who are active on Discord
  - Percentage who find Discord useful for learning

#### 3.2.5 Most Valuable Learning Methods
- Horizontal bar chart showing top 7 learning methods ranked by average rating (1-5 scale)
- Covers 10 different teaching/learning elements:
  - Explanations of pre-written code
  - Studying for midterms
  - TopHat Quizzes
  - Presentation slides
  - Post-class handouts and notes
  - Coding on own
  - Live coding by professor
  - Labs
  - Asking questions of professor during lecture
  - Assignments

#### 3.2.6 Community Connection Scores
- Horizontal bar chart showing average scores (1-5 scale) for community and belonging
- Covers 5 statements:
  - Comfort speaking up in class
  - Feeling like part of the class
  - Importance of making friends in class
  - Feeling part of the university community
  - Ease of meeting new people in class

### 3.3 Dynamic Chart Titles
- Each chart title dynamically updates to show "(Section Name)" when filtered, or "(All Sections)" when viewing all data

---

## 4. Question Responses Tab Features

### 4.1 Question Selector

**Free-Text Question Buttons**
- Horizontal row of clickable buttons, one for each free-text question
- Clicking a button:
  - Loads all responses for that specific question
  - Automatically switches to display the responses table
  - Updates the table caption to show the selected question

### 4.2 Responses Table

**Table Features:**
- Displays all responses for the selected question
- Each row represents one student's response
- Columns include:
  - Response ID (internal identifier, hidden from view)
  - The free-text response content
- Table is scrollable horizontally for long responses

**Table Controls:**
- **Sorting:** Click column headers to sort ascending/descending
- **Pagination:** Navigate through multiple pages of responses
- **Info Display:** Shows total number of responses and current view range

**Default Settings:**
- Shows 30 responses per page
- Ordering enabled

### 4.3 Individual Response Viewing

**Selecting a Response:**
- Click on an icon in each row in the responses table to select that student's response
- A modal dialog opens displaying the participant's full profile

**Participant Profile Modal:**
The modal displays comprehensive information about the selected student:

**Basic Information Section:**
- Section (e.g., "231 - 1pm")
- Prior Experience level
- Learning Preference

**Selected Response:**
- The full text of the response for the question the user was viewing

**All Other Responses:**
- Complete responses to all other free-text questions
- Only show the questions to which the student responded
- Allows viewing the student's complete survey in one place

**Modal Behavior:**
- Large size for comfortable reading
- Easy to close (click X or click outside)

---

## 5. Statistics Tab Features

The Statistics tab provides detailed statistical analysis of all Likert-scale questions in the survey.

### 5.1 Category Navigation

**Question Category Selector**
- Users can select which category of questions to analyze:
  - **Course Satisfaction** (6 questions)
  - **Learning Methods** (10 questions)
  - **Community & Belonging** (5 questions)

### 5.2 Question Selector

**Likert Question Buttons**
- Horizontal row of clickable buttons for each question within the selected category
- Each button displays a shortened version of the question
- Clicking a button loads detailed statistics and visualizations for that question

### 5.3 Statistical Summary Panel

For each selected question, users can view:

**Descriptive Statistics:**
- **N** - Number of valid responses
- **Mean** - Average score (1-5 scale)
- **Median** - Middle value
- **Mode** - Most frequent response
- **Standard Deviation** - Measure of spread
- **Standard Error** - Precision of the mean estimate
- **Minimum** - Lowest score
- **Maximum** - Highest score
- **Q1 (25th percentile)** - First quartile
- **Q3 (75th percentile)** - Third quartile
- **Missing** - Number of unanswered responses

### 5.4 Distribution Visualization

**Histogram Display:**
- Bar chart showing the distribution of responses across the 5-point scale
- Each bar shows:
  - Count of responses for that score
  - Percentage of total responses
- Color-coded bars (red to green gradient representing 1-5)
- X-axis shows the full Likert scale labels

### 5.5 Section Comparison Feature

**Compare Across Sections:**
- Toggle or button to enable section-by-section comparison
- When enabled, displays:
  - Separate histograms for each course section
  - Side-by-side statistical summaries
  - Allows comparison of how different sections responded to the same question

---

## 6. Insights Tab Features

The Insights tab provides advanced statistical analysis to uncover non-obvious patterns and relationships in the survey data.

### 6.1 Correlation Analysis

**Correlation Matrix View:**
- Visual display showing relationships between all Likert-scale questions
- Color-coded heatmap or matrix:
  - Green = positive correlation
  - Red = negative correlation
  - Intensity indicates strength
- Users can hover over cells to see exact correlation values
- Identifies which questions tend to be answered similarly

**Key Insights Display:**
- Automatically highlights the strongest positive correlations
- Automatically highlights the strongest negative correlations
- Helps identify which aspects of the course are related

### 6.2 Regression Analysis

**Predictive Model View:**
- Analysis showing which factors best predict overall satisfaction
- Displays:
  - List of predictor variables ranked by influence
  - Direction of effect (positive/negative)
  - Relative importance scores

### 6.3 Student Segmentation

**Cluster Analysis:**
- Automatic grouping of students based on their response patterns
- Displays:
  - Number of distinct student segments identified
  - Characteristics of each segment (e.g., "High Engagement", "Struggling", "Satisfied but Quiet")
  - Size of each segment (percentage of class)
- Visual representation of cluster profiles

### 6.4 Section Comparison Analysis

**Statistical Comparison:**
- Formal statistical tests comparing responses across sections
- Displays:
  - Which sections differ significantly
  - Effect sizes for meaningful differences
  - Visual comparison charts

### 6.5 Effect Size Analysis

**Practical Significance:**
- Shows which differences are not just statistically significant but practically meaningful
- Displays:
  - Effect size values (Cohen's d or similar)
  - Interpretation (small/medium/large effect)
  - Questions with the largest practical differences

### 6.6 Reliability Analysis

**Survey Consistency:**
- Measures how consistently students answered related questions
- Displays:
  - Cronbach's alpha or similar reliability metrics
  - Which question groups measure the same underlying concept
  - Internal consistency scores

### 6.7 Interaction Analysis

**Factor Interactions:**
- Shows how different factors combine to affect outcomes
- Displays:
  - Significant interactions between variables
  - Visual representations of interaction effects
  - Key findings about what combinations matter

### 6.8 Satisfaction Predictors

**Key Drivers Analysis:**
- Identifies which factors most strongly predict overall satisfaction
- Displays:
  - Ranked list of satisfaction drivers
  - Relative importance percentages
  - Actionable insights for course improvement

---

## 7. Data Filtering

### 7.1 Section-Based Filtering

**How Filtering Works:**
1. User clicks on a section bar in the "Responses per Section" chart
2. All data across all visualizations and tables filters to that section only
3. Chart titles update to show the selected section name
4. The "Selected Section" display updates
5. A "Reset Filter" button becomes active

**What Gets Filtered:**
- All six overview charts on Home tab
- Total response count
- Statistics calculations
- Insights analyses

**Resetting Filters:**
- Click "Reset Filter" button to return to viewing all sections
- Chart titles revert to "(All Sections)"

### 7.2 Category Filtering (Statistics/Insights Tabs)

- Users can filter to specific question categories
- Filters apply within each tab independently

---

## 8. User Interactions Summary

| Action | Result |
|--------|--------|
| Click section in bar chart | Filter all data to that section |
| Click "Reset Filter" | Clear section filter, show all data |
| Click question button (Home) | Load and display responses for that question |
| Click category tab (Statistics/Insights) | Switch between question categories |
| Click question button (Statistics) | Load statistics and histogram for that question |
| Toggle section comparison | Show/hide section-by-section breakdown |
| Type in search box | Filter responses containing search term |
| Click column header | Sort table by that column |
| Click table row | Open participant profile modal |
| Click modal X or outside | Close participant profile modal |
| Switch main tabs | Navigate between Home, Question Responses, Statistics, Insights |
| Hover over correlation cell | Show exact correlation value |
| Click cluster segment | View details about that student group |

---

## 9. Data Scope

Described in detail in data_details.md

---

## 10. Out of Scope

This document does not address:

- How data is loaded or processed
- Internal data structures or column mappings
- Server-side logic or reactivity
- Code organization or architecture
- Performance considerations
- Authentication or authorization
- Deployment configuration
- Specific statistical test implementations
- Machine learning model details

These implementation details are intentionally excluded to focus purely on the user experience for the purposes of planning a future refactoring.
