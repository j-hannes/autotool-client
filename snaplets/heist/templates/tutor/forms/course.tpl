<apply template="/tutor/base">

  <dfForm id="course-form" class="form-horizontal">

    <h2>Kurs erstellen</h2>

    <br />

    <dfChildErrorList class="alert alert-error" />

    <div class="control-group">
      <dfLabel class="control-label" ref="courseName">
        Name des Kurses
      </dfLabel>
      <div class="controls">
        <dfInputText ref="courseName" />
      </div>
    </div>


    <div class="control-group">
      <dfLabel class="control-label" ref="enrollmentOpening">
        Start der Einschreibung
      </dfLabel>
      <div class="controls">
        <div id="datetimepicker1" class="input-append date">
          <!--<input type="text"
                     data-format="dd/MM/yyyy hh:mm:ss"
                     value="13/02/2013 21:34:47" />-->
          <dfInputText ref="enrollmentOpening"
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
      <dfLabel class="control-label" ref="enrollmentDeadline">
        Ende der Einschreibung
      </dfLabel>
      <div class="controls">
        <div id="datetimepicker2" class="input-append date">
          <dfInputText ref="enrollmentDeadline"
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


    <div class="control-group">
      <dfLabel class="control-label" ref="passCriteria">
        Zugelassen bei
      </dfLabel>
      <div class="controls">
        <dfInputText class="input-mini" ref="passCriteria" />
      </div>
    </div>

    <h3>Übungsgruppe 1</h3>

    <div class="control-group">
      <dfLabel class="control-label" ref="group1Name">
        Beschreibung
      </dfLabel>
      <div class="controls">
        <dfInputText ref="group1Name" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="group1Capacity">
        Max. Studenten
      </dfLabel>
      <div class="controls">
        <dfInputText class="input-mini" ref="group1Capacity" />
      </div>
    </div>

    <h3>Übungsgruppe 2</h3>

    <div class="control-group">
      <dfLabel class="control-label" ref="group2Name">
        Beschreibung
      </dfLabel>
      <div class="controls">
        <dfInputText ref="group2Name" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="group2Capacity">
        Max. Studenten
      </dfLabel>
      <div class="controls">
        <dfInputText class="input-mini" ref="group2Capacity" />
      </div>
    </div>

    <a class="btn"
       href="/tutor">zurück</a>
    <dfInputSubmit class="btn btn-primary" value="erstellen" />

  </dfForm>

</apply>
