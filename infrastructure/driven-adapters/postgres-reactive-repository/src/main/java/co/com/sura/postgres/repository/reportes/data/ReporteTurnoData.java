package co.com.sura.postgres.repository.reportes.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "reportes_turno")
public class ReporteTurnoData {

    @Id
    private String id;

    @Column("fecha_turno")
    private LocalDate fechaTurno;

    @Column("id_regional")
    private String idRegional;

    @Column("id_horario_turno")
    private Integer idHorarioTurno;

    @Column("numero_profesionales")
    private Integer numeroProfesionales;

    @Column("horas_profesionales")
    private Double horasProfesionales;

    @Column("horas_asignadas")
    private Double horasAsignadas;

    @Column("horas_completadas")
    private Double horasCompletadas;

    @Column("citas_asignadas")
    private Integer citasAsignadas;

    @Column("citas_completadas")
    private Integer citasCompletadas;

    @Column("citas_canceladas")
    private Integer citasCanceladas;

    @Column("capacidad_actual")
    private Double capacidadActual;

}
