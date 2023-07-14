package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Cita {
    private String idCita;
    private Integer duracion;
    private Integer holgura;
    private LocalDateTime fechaInicio;
    private String especialidad;
    private Integer idEstado;
    private String idCiudad;
    private String idProfesional;
    private String idConductor;
}
