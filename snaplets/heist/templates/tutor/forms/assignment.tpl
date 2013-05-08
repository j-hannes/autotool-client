<apply template="/tutor/base">
  <dfForm id="assignment-form" class="form-horizontal">

    <h2>Aufgabe zuweisen</h2>

    <br />

    <div class="control-group">
      <dfLabel class="control-label" ref="course">
        Kurs
      </dfLabel>
      <div class="controls">
        <dfInputSelect ref="course" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="task">
        Aufgabe
      </dfLabel>
      <div class="controls">
        <dfInputSelect ref="task" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="status">
        Status
      </dfLabel>
      <div class="controls">
        <dfInputSelect ref="status" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="start">
        Veröffentlichung
      </dfLabel>
      <div class="controls">
        <div id="datetimepicker1" class="input-append date">
          <dfInputText ref="start"
                      data-format="dd/MM/yyyy hh:mm:ss" />
          <span class="add-on">
            <i data-time-icon="icon-time" data-date-icon="icon-calendar"></i>
          </span>
        </div>
      </div>
    </div>

    <script>
      $(function() {
        $('#datetimepicker1').datetimepicker({
          language: 'de'
        });
      });
    </script>

    <div class="control-group">
      <dfLabel class="control-label" ref="end">
        Deadline
      </dfLabel>
      <div class="controls">
        <div id="datetimepicker2" class="input-append date">
          <dfInputText ref="end"
                      data-format="dd/MM/yyyy hh:mm:ss" />
          <span class="add-on">
            <i data-time-icon="icon-time" data-date-icon="icon-calendar"></i>
          </span>
        </div>
      </div>
    </div>

    <script>
      $(function() {
        $('#datetimepicker2').datetimepicker({
          language: 'de'
        });
      });
    </script>

    <dfChildErrorList class="alert alert-error" />

    <a class="btn"
       href="/tutor">zurück</a>
    <dfInputSubmit class="btn btn-primary" value="erstellen" />
  </dfForm>
</apply>
