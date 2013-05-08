<apply template="/base">
  <bind tag="header">
    <div class="navbar navbar-static-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="brand" href="/">autotool'</a>
          <ul class="nav">
            <li><a href="/tutor">Tutor</a></li>
            <li><a href="/student/select">Student</a></li>
          </ul>
        </div>
      </div>
    </div>
  </bind>

  <div class="container">
    <h1>Select Student</h1>
    <div class="row select-student">
      <div class="span3">
        <a href="/student/1" class="hero-unit">
          <h1>1</h1>
        </a>
      </div>
      <div class="span3">
        <a href="/student/2" class="hero-unit">
          <h1>2</h1>
        </a>
      </div>
      <div class="span3">
        <a href="/student/3" class="hero-unit">
          <h1>3</h1>
        </a>
      </div>
      <div class="span3">
        <a href="/student/4" class="hero-unit">
          <h1>4</h1>
        </a>
      </div>
    </div>
  </div>

  <bind tag="footer">
  </bind>
</apply>
