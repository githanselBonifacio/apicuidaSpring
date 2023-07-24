package co.com.sura.entity.remision;

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
    private LocalDateTime fechaInicio;
    private LocalDateTime fechaProgramada;
    private Double latitud;
    private Double longitud;
    private String especialidad;
    private Integer idEstado;
    private String idCiudad;
    private String idProfesional;
    private String idConductor;
}
