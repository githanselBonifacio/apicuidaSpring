package co.com.sura.postgres.personal.adapter;

import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.personal.entity.Conductor;
import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.personal.entity.ProfesionalWithTurno;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.remision.entity.datosremision.ItemSecuenciaTurno;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.personal.data.ConductorData;
import co.com.sura.postgres.personal.data.ItemSecuenciaTurnoData;
import co.com.sura.postgres.personal.data.MovilData;
import co.com.sura.postgres.personal.data.ProfesionalData;
import co.com.sura.postgres.personal.data.TurnoProfesionalesData;

import java.util.Collections;

public class ConverterPersonal extends Converter {



    public static Profesional convertToProfesional(ProfesionalData profesionalData){
        return converToEntity(profesionalData, Profesional.class);
    }
    public static ProfesionalData convertToProfesionalData(Profesional profesional){
        return converToEntity(profesional, ProfesionalData.class);
    }

    public static ProfesionalWithTurno convertToProfesionalTurno(ProfesionalData profesionalData){
        return converToEntity(profesionalData, ProfesionalWithTurno.class);
    }
    public static TurnoProfesional convertToTurnoProfesional(TurnoProfesionalesData turnoProfesionalesData){
        return converToEntity(turnoProfesionalesData, TurnoProfesional.class);
    }
    public static TurnoProfesionalesData convertToTurnoProfesionalData(TurnoProfesional turnoProfesional){
        return converToEntity(turnoProfesional, TurnoProfesionalesData.class);
    }

    public  static Conductor converToConductor(ConductorData conductorData){
        return converToEntity(conductorData, Conductor.class);
    }
    public  static ConductorData converToConductorData(Conductor conductor){
        return converToEntity(conductor, ConductorData.class);
    }
    public static Movil convertToMovil(MovilData movilData){
        return  converToEntity(movilData, Movil.class);
    }
    public static MovilData convertToMovilData(Movil movil){
        return   converToEntity(movil, MovilData.class);
    }

    public static ItemSecuenciaTurno convertToSecuenciaTurno(ItemSecuenciaTurnoData secuenciaTurnoData){
        return ItemSecuenciaTurno
                .builder()
                .nombreSecuencia(secuenciaTurnoData.getNombreSecuencia())
                .descripcion(secuenciaTurnoData.getDescripcion())
                .numeroDia(secuenciaTurnoData.getNumeroDia())
                .nombreDia(secuenciaTurnoData.getNombreDia())
                .horariosTurno(Collections.singletonList(
                        HorarioTurno.builder().id(secuenciaTurnoData.getIdHorarioTurno()).build()))
                .build();
    }
}
