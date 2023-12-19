package co.com.sura.agenda.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@SuperBuilder(toBuilder = true)
public class Cita {
    private String idCita;
    private String idRemision;
    private Integer duracion;
    private Integer holgura;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    private LocalDateTime fechaInicio;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    private LocalDateTime fechaProgramada;

    private Double latitud;
    private Double longitud;
    private String especialidad;
    private Integer idEstado;
    private String idRegional;
    private Integer idHorarioTurno;
    private String idProfesional;
    private String idConductor;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String paciente;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String numeroIdentificacionPaciente;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String tipoIdentificacionPaciente;
}
