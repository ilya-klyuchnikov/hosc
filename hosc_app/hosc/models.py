from google.appengine.ext import db

class Author(db.Model):
    user = db.UserProperty()
    n_programs = db.IntegerProperty()

class Program(db.Model):
    name = db.StringProperty()
    summary = db.StringProperty()
    author = db.ReferenceProperty(Author) # ==parent
    code = db.TextProperty()
    notes = db.TextProperty()
    date = db.DateTimeProperty(auto_now_add=True) #creation date
    modified = db.DateTimeProperty(auto_now=True)
    scp_code = db.TextProperty()
    svg_tree = db.TextProperty()
    
class Test(db.Model):
    name = db.StringProperty()
    summary = db.StringProperty()
    author = db.ReferenceProperty(Author) # ==parent
    types = db.TextProperty()
    goal1 = db.TextProperty()
    goal2 = db.TextProperty()
    defs = db.TextProperty()
    date = db.DateTimeProperty(auto_now_add=True) #creation date
    modified = db.DateTimeProperty(auto_now=True)
    scp_code1 = db.TextProperty()
    scp_code2 = db.TextProperty()
    eq = db.BooleanProperty()
    notes = db.TextProperty()
    
class Comment(db.Model):
    program = db.ReferenceProperty(Program, collection_name='comments')
    text = db.TextProperty()
    author = db.ReferenceProperty(Author)
    created = db.DateTimeProperty(auto_now_add=True) #creation date
    modified = db.DateTimeProperty(auto_now=True)
    
def get_author_for_user(user):
    author = db.Query(Author).filter('user =', user).get()
    author = Author.get_by_key_name(user.email())
    if author is not None:
        return author
    else:
        return Author.get_or_insert(user.email(), user=user, n_programs=0)
        
    
def _add_program_for_author(author_key, name=None, summary=None, code=None, notes=None, scp_code=None, svg_tree=None):
    author = db.get(author_key)
    author.n_programs = author.n_programs + 1
    program = Program(parent=author)
    program.name = name
    program.summary = summary
    program.author = author
    program.code = code
    program.notes = notes
    program.scp_code = scp_code
    program.svg_tree = svg_tree
    program.put()
    author.put()
    
def add_test_for_user(author_key, name=None, summary=None, 
                        types=None, goal1=None, goal2=None, defs=None, 
                        scp_code1=None, scp_code2=None, eq=None, notes=None):
    author = db.get(author_key)
    test = Test(parent=author)
    test.name = name
    test.summary = summary
    test.author = author
    test.types = types
    test.goal1 = goal1
    test.goal2 = goal2
    test.defs = defs
    test.scp_code1 = scp_code1
    test.scp_code2 = scp_code2
    test.eq = eq
    test.notes = notes
    test.put()
    
def add_program_for_user(author_key, name=None, summary=None, code=None, notes=None, scp_code=None, svg_tree=None):
    db.run_in_transaction(_add_program_for_author, author_key, name, summary, code, notes, scp_code, svg_tree)

def _delete_program(program):
    author = program.author
    author.n_programs = author.n_programs - 1
    program.delete()
    author.put()
    
def delete_program(program):
    db.run_in_transaction(_delete_program, program)