# Global variables and data loading

# Define free-text questions
free_text_questions <- c(
  "free_text_learning_preference" = "Learning Preference",
  "free_text_challenge_meeting_people" = "Challenge Meeting People",
  "free_text_class_welcoming_inclusive" = "Class Welcoming & Inclusive",
  "free_text_class_interesting_engaging" = "Class Interesting & Engaging",
  "free_text_learning_meeting_expectations" = "Learning Meeting Expectations",
  "free_text_hopes_interacting_students" = "Hopes Interacting with Students",
  "free_text_hopes_interacting_professor" = "Hopes Interacting with Professor",
  "free_text_class_favorite_part" = "Class Favorite Part",
  "free_text_class_least_enjoyable_part" = "Class Least Enjoyable Part",
  "free_text_more_and_less_of" = "More and Less Of",
  "discord_other_text" = "Discord Other Text"
)

# Define standardized column names for survey questions
# These match the lowercase-converted column names from the CSV file
survey_columns <- list(
  course = c(
    excited = "x.course..i.am.excited.about.the.content.and.material.that.i.m.learning",
    relevant = "x.course..the.content.is.relevant.and.up.to.date",
    meeting_goals = "x.course..i.feel.like.i.am.meeting.the.goals.of.learning.python.in.this.course",
    apply_scenario = "x.course..i.feel.like.i.could.take.what.i.m.learning.and.apply.it.in.a.new.scenario",
    feedback = "x.course..i.m.satisfied.with.the.level.of.feedback.i.receive",
    ask_help = "x.course..it.s.easy.to.ask.for.help"
  ),
  learning = c(
    pre_written_code = "x.learning..explanations.of.pre.written.code",
    live_coding = "x.learning..live.coding.by.the.professor",
    slides = "x.learning..presentation.slides",
    handouts = "x.learning..post.class.handouts.and.notes",
    tophat_quizzes = "x.learning..tophat.quizzes",
    assignments = "x.learning..assignments",
    labs = "x.learning..labs",
    ask_questions = "x.learning..being.able.to.ask.questions.of.the.professor.during.lecture",
    studying_midterms = "x.learning..studying.for.midterms",
    coding_own = "x.learning..coding.on.my.own"
  ),
  community = c(
    friends_important = "x.community..making.friends.within.the.class.is.important.to.me",
    easy_meet = "x.community..it.s.easy.to.meet.new.people.within.the.class",
    part_of_class = "x.community..i.feel.like.i.am.a.part.of.this.class",
    comfortable_speaking = "x.community..i.feel.comfortable.speaking.up.in.class",
    part_of_university = "x.community..i.feel.like.i.am.a.part.of.the.university.community"
  )
)