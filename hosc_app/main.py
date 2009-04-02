from hosc import views
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util

application = webapp.WSGIApplication(
                                     [('/', views.Root),
                                      ('/all', views.All),
                                      ('/supercompiler', views.Supercompiler),
                                      ('/edit', views.Edit),
                                      ('/delete', views.Delete),
                                      ('/view', views.Get),
                                      ('/svg', views.Svg),
                                      ('/svgpreview', views.SvgPreview),
                                      ('/authors', views.Authors),
                                      ('/author', views.Author),
                                      ('/mine', views.Mine),
                                      ('/eq', views.Eq),
                                      ('/tests', views.Tests),
                                      ('/tedit', views.TEdit),
                                      ('/tdelete', views.TDelete),
                                      ('/test', views.Test)]
                                     )

def main():
  util.run_wsgi_app(application)

if __name__ == "__main__":
  main()