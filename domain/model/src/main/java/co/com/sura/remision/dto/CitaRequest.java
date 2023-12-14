package co.com.sura.remision.dto;

import co.com.sura.remision.entity.datosremision.AlertaCita;
import com.fasterxml.jackson.annotation.JsonFormat;
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
public class CitaRequest {
    private String idCita;
    private Integer duracion;
    private Integer holgura;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    private LocalDateTime fechaInicio;
    private String especialidad;
    private AlertaCita alertaCita;
    private List<TratamientoRequest> tratamientos;
    private ProcedimientoRequest procedimientos;
}
