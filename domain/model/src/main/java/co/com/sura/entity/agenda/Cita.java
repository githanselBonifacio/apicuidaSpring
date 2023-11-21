package co.com.sura.entity.agenda;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
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
    private String idProfesional;
    private String idConductor;
    private String paciente;
    private String numeroIdentificacionPaciente;
    private String tipoIdentificacionPaciente;
}
