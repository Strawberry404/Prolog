import streamlit as st
import subprocess
import re
import pandas as pd
import os

# Set page configuration with some pizzazz!
st.set_page_config(
    page_title="Student Merit System",
    page_icon="🧠",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Add some style - because why be boring?
st.markdown("""
<style>
    .main-header {
        font-size: 42px;
        background: linear-gradient(to right, #4e8df5, #c471ed);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        padding-bottom: 10px;
    }
    .subheader {
        font-size: 20px;
        color: #636EFA;
        font-style: italic;
    }
    .metric-card {
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    }
    .student-row {
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 5px;
    }
    .student-row:hover {
        background-color: #f0f2f6;
    }
</style>
""", unsafe_allow_html=True)

# Our glorious header
st.markdown('<p class="main-header">Student Merit Ranking System</p>', unsafe_allow_html=True)
st.markdown('<p class="subheader">Where logic meets beauty, and Prolog meets Python!</p>', unsafe_allow_html=True)

# Create the Prolog code file if it doesn't exist yet
def initialize_prolog_system():
    with open('SystemeExpert.pl', 'w') as f:
        f.write("""
% Déclaration des prédicats dynamiques
:- dynamic etudiant/6.

% Définition des constantes
salaire_minimum(10000).
grand_famille(3).
ecole_postal(90000).

% Définir une structure pour représenter un étudiant
etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score).

% Règle 1: salaire 
points_salaire(Revenu, Points) :- 
    salaire_minimum(Min),
    (Revenu =< Min -> Points = 2 ;
     Revenu =< 2*Min -> Points = 1 ;
     Points = 0).

% Règle 2: zone postal 
points_postal(CodePostal, Points) :- 
    ecole_postal(Code),
    (CodePostal = Code -> Points = 3 ;
     CodePostal =:= Code+1 -> Points = 2 ;
     CodePostal =:= Code-1 -> Points = 2;
     Points = 0).

% Règle 3: nbr de frère et soeur
points_famille(NbFreres, Points) :- 
    grand_famille(Seuil),
    (NbFreres > Seuil -> Points = 1.5 ;
     Points = 0).

% Règle 4: des handicap
points_handicap(true, 1.5).
points_handicap(false, 0).

% Règle 5: calcul de score 
calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score) :-
    points_salaire(Revenu, S1),
    points_postal(CodePostal, S2),
    points_famille(NbFreres, S3),
    points_handicap(Handicap, S4),
    Score is S1 + S2 + S3 + S4.

% Ajouter un étudiant
ajouter_etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap) :-
    calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score),
    assertz(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)).

% Supprimer un étudiant
supprimer_etudiant(Nom) :-
    retract(etudiant(Nom, _, _, _, _, _)).

% Récupérer tous les étudiants
get_all_students(Students) :-
    findall(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score),
            etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score),
            Students).

% Obtenir la liste triée
get_sorted_students(SortedStudents) :-
    findall(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)-Score,
            etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score),
            Students),
    sort(2, @>=, Students, SortedStudents).

% Sauvegarder les données dans un fichier
save_data :-
    tell('student_data.pl'),
    listing(etudiant/6),
    told.

% Charger les données depuis un fichier
load_data :-
    [student_data].
        """)
    
    # Create database file if it doesn't exist
    if not os.path.exists('student_data.pl'):
        with open('student_data.pl', 'w') as f:
            f.write('% Student data file - automatically generated\n')

# Initialize our Prolog brain
initialize_prolog_system()

# Our magical bridge to the Prolog universe
def run_prolog_query(query):
    try:
        # The subprocess.run is like a portal between dimensions!
        result = subprocess.run(
            ['swipl', '-q', '-s', 'merit_system.pl', '-g', query, '-t', 'halt'],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        st.error(f"⚠️ Prolog Error: {e.stderr}")
        return None
    except Exception as e:
        st.error(f"⚠️ Error: {str(e)}")
        return None

# Load our brilliant students from the Prolog dimension
def fetch_students():
    # First, make sure we load any saved students
    run_prolog_query("load_data")
    
    # Ask Prolog for all students
    result = run_prolog_query("get_all_students(Students), write(Students).")
    
    students = []
    if result and result.strip():
        # Parse the Prolog response - it's like translating an ancient text!
        matches = re.findall(r'etudiant\(([^,]+),\s*(\d+),\s*(\d+),\s*(\d+),\s*(true|false),\s*([0-9.]+)\)', result)
        for match in matches:
            name, income, postal, siblings, disability, score = match
            students.append({
                'name': name,
                'income': int(income),
                'postal_code': int(postal),
                'siblings': int(siblings),
                'disability': disability == 'true',
                'score': float(score)
            })
    
    return students

# Add a student to our magical database
def add_student(name, income, postal_code, siblings, disability):
    disability_value = "true" if disability else "false"
    query = f"ajouter_etudiant('{name}', {income}, {postal_code}, {siblings}, {disability_value}), save_data."
    run_prolog_query(query)

# Remove a student (oh no!)
def remove_student(name):
    query = f"supprimer_etudiant('{name}'), save_data."
    run_prolog_query(query)

# Create a sidebar for navigation - every good app needs one!
st.sidebar.image("https://via.placeholder.com/150?text=Merit+System", width=150)
st.sidebar.markdown("## Navigation")
page = st.sidebar.radio(
    "Choose a page:",
    ["📝 Add Students", "🏆 View Rankings", "ℹ️ About System"]
)

# Explain the scoring system in a fun, visual way
def show_scoring_explanation():
    st.markdown("### 🧮 The Secret Formula Behind Our Scores")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("""
        #### 💰 Income Points (0-2)
        - **Poor families (≤10,000)**: 2 points
          _Because education should lift all boats!_
        - **Middle income (≤20,000)**: 1 point
          _A little help goes a long way_
        - **Higher income (>20,000)**: 0 points
          _These families have resources_
        
        #### 🏠 Postal Code Points (0-3)
        - **Same as school (90000)**: 3 points
          _Local community first!_
        - **Adjacent areas (89999, 90001)**: 2 points
          _Close neighbors matter too_
        - **Other areas**: 0 points
          _Distance makes things harder_
        """)
    
    with col2:
        st.markdown("""
        #### 👨‍👩‍👧‍👦 Siblings Points (0-1.5)
        - **Large families (>3 siblings)**: 1.5 points
          _Multiple school fees can be challenging_
        - **Smaller families**: 0 points
          _Fewer educational expenses_
        
        #### ♿ Disability Points (0-1.5)
        - **Family with disability**: 1.5 points
          _Extra challenges deserve extra support_
        - **No disability**: 0 points
          _Standard situation_
        """)
    
    st.markdown("### 🔢 Maximum Possible Score: 8 points")
    
    # A fun progress bar visualization
    st.markdown("#### Score Component Breakdown")
    st.progress(2/8, text="Income (max 2 points)")
    st.progress(3/8, text="Postal Code (max 3 points)")
    st.progress(1.5/8, text="Siblings (max 1.5 points)")
    st.progress(1.5/8, text="Disability (max 1.5 points)")

# Display different pages based on selection
if page == "📝 Add Students":
    st.header("Add New Students")
    
    # Create state variables if they don't exist
    if 'preview_income' not in st.session_state:
        st.session_state.preview_income = 10000
    if 'preview_postal' not in st.session_state:
        st.session_state.preview_postal = 90000
    if 'preview_siblings' not in st.session_state:
        st.session_state.preview_siblings = 0
    if 'preview_disability' not in st.session_state:
        st.session_state.preview_disability = False
    
    # Preview calculation OUTSIDE any form
    st.subheader("Score Preview Calculator")
    preview_col1, preview_col2 = st.columns(2)
    
    with preview_col1:
        st.session_state.preview_income = st.number_input("Preview Income 💰", 
                                                         min_value=0, 
                                                         value=st.session_state.preview_income, 
                                                         step=1000, 
                                                         key="preview_income_input")
        st.session_state.preview_postal = st.number_input("Preview Postal Code 📮", 
                                                         min_value=0, 
                                                         value=st.session_state.preview_postal,
                                                         key="preview_postal_input")
    
    with preview_col2:
        st.session_state.preview_siblings = st.number_input("Preview Siblings 👨‍👩‍👧‍👦", 
                                                          min_value=0, 
                                                          value=st.session_state.preview_siblings,
                                                          key="preview_siblings_input")
        st.session_state.preview_disability = st.checkbox("Preview Disability ♿", 
                                                        value=st.session_state.preview_disability,
                                                        key="preview_disability_input")
    
    # THIS button is fine because it's outside any form
    if st.button("Calculate Preview Score", type="secondary"):
        # Preview calculation logic
        score = 0
        if st.session_state.preview_income <= 10000:
            score += 2
        elif st.session_state.preview_income <= 20000:
            score += 1
        
        if st.session_state.preview_postal == 90000:
            score += 3
        elif st.session_state.preview_postal in [89999, 90001]:
            score += 2
        
        if st.session_state.preview_siblings > 3:
            score += 1.5
        
        if st.session_state.preview_disability:
            score += 1.5
            
        st.info(f"Estimated preview score: {score} / 8")
    
    st.markdown("---")
    
    # Actual student addition form - completely separate!
    st.subheader("Add New Student")
    st.markdown("Fill in student details below:")
    
    with st.form(key="student_form"):
        col1, col2 = st.columns(2)
        
        with col1:
            name = st.text_input("Student Name 👤")
            income = st.number_input("Family Income 💰", min_value=0, value=10000, step=1000)
            postal_code = st.number_input("Postal Code 📮", min_value=0, value=90000)
        
        with col2:
            siblings = st.number_input("Number of Siblings 👨‍👩‍👧‍👦", min_value=0, value=0)
            disability = st.checkbox("Family Member with Disability ♿")
        
        # The one and only button in the form - a submit button
        submit_button = st.form_submit_button("Add Student 🚀")
    
    # Process submission logic
    if submit_button:
        if name:
            add_student(name, income, postal_code, siblings, disability)
            st.success(f"✅ {name} has been added to the system! Their application will be considered.")
            st.rerun()

        else:
            st.warning("⚠️ Please enter a student name.")
    
    # Rest of the student display code stays the same...  
elif page == "🏆 View Rankings":
    st.header("Student Merit Rankings")
    
    students = fetch_students()
    if students:
        # Sort students by score (highest first)
        students.sort(key=lambda x: x['score'], reverse=True)
        
        # Create and style a dataframe
        df = pd.DataFrame(students)
        df.columns = ['Name', 'Income', 'Postal Code', 'Siblings', 'Disability', 'Score']
        df.index = range(1, len(df) + 1)  # 1-based ranking
        
        # Make the table look pretty
        st.dataframe(
            df.style.background_gradient(subset=['Score'], cmap='viridis')
                   .format({'Score': '{:.2f}'})
                   .highlight_max(subset=['Score'], color='lightgreen')
                   .highlight_min(subset=['Score'], color='lightpink'),
            use_container_width=True
        )
        
        # Some statistics to make it interesting
        st.subheader("Quick Stats")
        metrics_col1, metrics_col2, metrics_col3, metrics_col4 = st.columns(4)
        
        with metrics_col1:
            st.metric("Highest Score", f"{max([s['score'] for s in students]):.2f}")
        
        with metrics_col2:
            st.metric("Average Score", f"{sum([s['score'] for s in students]) / len(students):.2f}")
        
        with metrics_col3:
            st.metric("Total Students", len(students))
        
        with metrics_col4:
            # Calculate how many would be accepted if we had a threshold of 5
            accepted = sum(1 for s in students if s['score'] >= 5)
            st.metric("Accepted (Score ≥ 5)", accepted)
        
        # Top Students section
        if len(students) >= 3:
            st.subheader("🏅 Top 3 Students")
            top3_col1, top3_col2, top3_col3 = st.columns(3)
            
            with top3_col1:
                st.markdown(f"### 🥇 {students[0]['name']}")
                st.markdown(f"**Score: {students[0]['score']:.2f}**")
            
            with top3_col2:
                if len(students) > 1:
                    st.markdown(f"### 🥈 {students[1]['name']}")
                    st.markdown(f"**Score: {students[1]['score']:.2f}**")
            
            with top3_col3:
                if len(students) > 2:
                    st.markdown(f"### 🥉 {students[2]['name']}")
                    st.markdown(f"**Score: {students[2]['score']:.2f}**")
        
        # Show the scoring explanation
        with st.expander("How are scores calculated?"):
            show_scoring_explanation()
        
    else:
        st.info("No students in the system yet. Add some students to see rankings!")
        
        # Even with no students, show the scoring system
        show_scoring_explanation()

else:  # About System page
    st.header("About the Merit Ranking System")
    
    st.markdown("""
    ## 🧠 The Philosophy Behind Our System
    
    This Merit Ranking System marries the logical prowess of **Prolog** with the user-friendly elegance of **Python's Streamlit**. It's like having the brain of a mathematician and the charm of a host at your service!
    
    ### 🔄 How This Hybrid System Works
    
    Under the hood, we're running a fascinating cross-language dance:
    
    1. **Prolog** handles all the logical reasoning and score calculation - it's our logical brain!
    2. **Python** with **Streamlit** provides this beautiful interface you're interacting with - it's our friendly face!
    3. When you add or view students, Python sends messages to Prolog, which does the heavy lifting and sends back results.
    
    ### 🎯 Purpose of This System
    
    The system aims to create a fair, transparent process for ranking students based on various socioeconomic factors:
    
    - **Family income** - giving priority to those with financial need
    - **Geographic proximity** - considering distance from the school
    - **Family size** - accounting for families with multiple children
    - **Disability status** - providing additional support where needed
    
    ### 🛠️ Technical Details
    
    For the technically curious, this system demonstrates:
    
    - Inter-process communication between Python and Prolog
    - Dynamic knowledge base manipulation in Prolog
    - Reactive web interface with Streamlit
    - Multi-criteria decision making
    """)
    
    # A fun "architecture" diagram
    st.subheader("System Architecture")
    st.code("""
    ┌─────────────────┐       ┌───────────────────┐
    │                 │       │                   │
    │  Streamlit UI   │◄─────►│   Python Bridge   │
    │  (This page!)   │       │                   │
    │                 │       └────────┬──────────┘
    └─────────────────┘                │
                                       │
                                       ▼
                            ┌─────────────────────┐
                            │                     │
                            │   Prolog Engine     │
                            │  (Logical Brain)    │
                            │                     │
                            └──────────┬──────────┘
                                       │
                                       ▼
                            ┌─────────────────────┐
                            │                     │
                            │  Student Database   │
                            │  (student_data.pl)  │
                            │                     │
                            └─────────────────────┘
    """)
    
    # Credits section
    st.subheader("Credits & Inspiration")
    st.markdown("""
    This system draws inspiration from:
    
    - Logic programming principles in education management
    - Multi-criteria decision analysis techniques
    - Human-centered design for accessibility
    
    Built with ❤️ using Prolog's logical magic and Streamlit's visual charm!
    """)

# Add a cute footer with the date
st.markdown("---")
st.markdown("*Merit Ranking System v1.0 • Made with logical love 🧠❤️*")