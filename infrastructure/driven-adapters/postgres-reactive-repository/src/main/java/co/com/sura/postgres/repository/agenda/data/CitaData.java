package co.com.sura.postgres.repository.agenda.data;

import co.com.sura.genericos.EstadosCita;
import co.com.sura.genericos.Numeros;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;
import javax.persistence.Entity;
import java.time.LocalDateTime;
import java.util.Collection;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "citas")
public class CitaData {
    @Id
    @Column("id_cita")
    private String idCita;
    @Column("id_remision")
    private String idRemision;
    private Integer duracion;
    private Integer holgura;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    @Column("fecha_inicio")
    private LocalDateTime fechaInicio;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    @Column("fecha_programada")
    private LocalDateTime fechaProgramada;
    private String especialidad;
    @Column("id_regional")
    private String idRegional;
    @Column("id_horario_turno")
    private Integer idHorarioTurno;
    @Column("id_estado")
    private Integer idEstado;
    @Column("id_profesional")
    private String idProfesional;
    @Column("id_conductor")
    private String idConductor;
    @Column("latitud")
    private Double latitud;
    @Column("longitud")
    private Double longitud;

    public static Double duracionTotalCitas(Collection<CitaData> citas){
        return citas
                .stream()
                .mapToDouble(CitaData::getDuracion)
                .map(value->value/Numeros.SEGUNDOS_EN_HORAS.getValue())
                .sum();
    }

    public static Double horasCompletadasCitas(Collection<CitaData> citas){
        return citas
                .stream()
                .filter(cita -> cita.getIdEstado() == EstadosCita.FINALIZADA.getEstado())
                .mapToDouble(CitaData::getDuracion)
                .map(value->value/Numeros.SEGUNDOS_EN_HORAS.getValue())
                .sum();

    }
    public static int citasCompletadas(Collection<CitaData> citaData){
       return (int) citaData
               .stream()
               .filter(cita -> cita.getIdEstado() == EstadosCita.FINALIZADA.getEstado())
               .count();
    }

    public static Integer citasCanceladas(Collection<CitaData> citaData){
        return (int) citaData
                .stream()
                .filter(cita -> cita.getIdEstado() == EstadosCita.CANCELADA.getEstado())
                .count();
    }

    public static Double capacidadTurno(Double horasAsigandas,Double horasDisponibles){
        if (horasDisponibles == 0) {
            return  null;
        } else {
            return  (horasAsigandas) /
                    (horasDisponibles) * Numeros.CIEN.getValue();
        }
    }
}
