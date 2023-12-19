package co.com.sura.postgres.agenda.data;

import co.com.sura.genericos.EstadosCita;
import co.com.sura.genericos.Numeros;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
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
public class CitaData  {
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
                .map(value->value/Numeros.SEGUNDOS_EN_HORAS)
                .sum();
    }

    public static Double horasCompletadasCitas(Collection<CitaData> citas){
        return citas
                .stream()
                .filter(cita -> cita.getIdEstado() == EstadosCita.FINALIZADA.getEstado())
                .mapToDouble(CitaData::getDuracion)
                .map(value->value/Numeros.SEGUNDOS_EN_HORAS)
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
                    (horasDisponibles) * Numeros.CIEN;
        }
    }

    public static Boolean validarFechasToReprogramar(CitaData citaFechaInferior,
                                                     CitaData citaFechaSuperior,
                                                     CitaData citaAsignada,
                                                     DesplazamientoData desplazamientoCitaInferior,
                                                     DesplazamientoData desplazamientoCitaAsignada,
                                                     DesplazamientoData desplazamientoSede,
                                                     LocalDateTime fechaProgramada){


        var validarDesplazamientoDesdeSede  = true;
        if(desplazamientoSede.getFechaProgramada()!=null){
             validarDesplazamientoDesdeSede = fechaProgramada
                    .isAfter(desplazamientoSede.getFechaProgramada().plusSeconds(desplazamientoSede.getDuracion()));
        }
        boolean validacionFechaAnterior = citaFechaInferior.getFechaProgramada() == null ||
                citaFechaInferior.getFechaProgramada().plusSeconds(citaFechaInferior.getDuracion())
                        .plusSeconds(desplazamientoCitaInferior.getDuracion())
                        .isBefore(fechaProgramada);

        boolean validacionFechaPosterior = citaFechaSuperior.getFechaProgramada() == null ||
                citaFechaSuperior.getFechaProgramada()
                        .isAfter(fechaProgramada.plusSeconds(citaAsignada.getDuracion())
                                .plusSeconds(desplazamientoCitaAsignada.getDuracion()));

        return validacionFechaAnterior && validacionFechaPosterior && validarDesplazamientoDesdeSede;
    }
    public static Boolean validarDisponibilidadFechasToAgendar(CitaData citaFechaInferior, CitaData citaFechaSuperior,
                                                               CitaData citaAgendada,
                                                               DesplazamientoData desplazamientoCitaInferior,
                                                               DesplazamientoData desplazamientoCitaAsignada,
                                                              DesplazamientoData desplazamientoSede){

        var validarDesplazamientoDesdeSede  = true;
        if (citaFechaInferior.getFechaProgramada() == null && desplazamientoSede.getFechaProgramada()!=null){
             validarDesplazamientoDesdeSede = citaAgendada.getFechaProgramada()
                    .isAfter(desplazamientoSede.getFechaProgramada().plusSeconds(desplazamientoSede.getDuracion()));
        }


        boolean validacionFechaAnterior = citaFechaInferior.getFechaProgramada() == null ||
                citaFechaInferior.getFechaProgramada()
                        .plusSeconds(citaFechaInferior.getDuracion())
                        .plusSeconds(desplazamientoCitaInferior.getDuracion())
                        .isBefore(citaAgendada.getFechaProgramada());

        boolean validacionFechaPosterior = citaFechaSuperior.getFechaProgramada() == null ||
                citaFechaSuperior.getFechaProgramada()
                        .isAfter(citaAgendada.getFechaProgramada()
                                .plusSeconds(citaAgendada.getDuracion())
                                .plusSeconds(desplazamientoCitaAsignada.getDuracion()));

        return validacionFechaAnterior && validacionFechaPosterior && validarDesplazamientoDesdeSede;
    }
}
