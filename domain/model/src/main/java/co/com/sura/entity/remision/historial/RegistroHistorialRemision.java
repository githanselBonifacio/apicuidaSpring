package co.com.sura.entity.remision.historial;

import co.com.sura.entity.remision.datosremision.Paciente;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RegistroHistorialRemision {

    private String idRemision;
    private String estado;
    private LocalDateTime fechaAplicacionNovedad;
    private String motivoNovedad;
    private LocalDate fechaAdmision;
    private String programa;
    private String tipoAdmision;
    private String institucionRemite;
    private Paciente paciente;
    private Object ubicacionPaciente;
    private Object datosAtencion;
    private Object diagnosticos;
    private List<CitaHistorial> citas;
    private List<CitaHistorial> citasNuevas;
}
